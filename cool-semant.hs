-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolSemant (
		Result(..),
		lookupClass,
		semant
	) where
import CoolAST
import CoolBasic
import CoolLexer
import qualified CoolParser (Result(..),parse)
import Control.Monad (unless,when)
import Data.Either (lefts,rights)
import Data.List (find,group,groupBy,sort)
import Data.Maybe (fromJust,isJust)

-- The data type representing a result of semantic analysis. Can be used
-- as a monad if appropriate bind and return operations are provided.
data Result a =
	LexerError Int String		|
	ParserError Int String		|
	SemanticError Int String	|
	GenericError String			|
	Program a

-- Make the above a monadic data type.
instance Monad Result where
	(>>=) = bnd
	return = ret

-- Monadic bind operation.
bnd :: Result a -> (a -> Result b) -> Result b
bnd (SemanticError l m) _ = SemanticError l m
bnd (GenericError m) _ = GenericError m
bnd (Program x) k = k x

-- Monadic return operation.
ret :: a -> Result a
ret x = Program x

-- This fully represents a type environment as described in the Cool
-- manual with the current class name, an object environment and a
-- method environment. It also contains a full representation of the
-- inheritance relationship as a fourth element.
type Environment = (Type, ObjectEnv, MethodEnv, [(Type,Type)])
type ObjectEnv = [(String,Type)]
type MethodEnv = [((Type,String),[Type])]

envC :: Environment -> Type
envC (c,_,_,_) = c

envO :: Environment -> String -> Result Type
envO (_,o,_,_) s = maybe (errUndeclaredO s) ret (lookup s o)

envM :: Environment -> Type -> String -> Result [Type]
envM (_,_,m,_) t s = maybe (errUndeclaredM s) ret (lookup (t,s) m)

envI :: Environment -> Type -> Maybe Type
envI (_,_,_,i) t = lookup t i

envPushO :: ObjectEnv -> Environment -> Environment
envPushO o1 (c,o2,m,i) = (c,o1++o2,m,i)

-- Predefined error messages that are presented to the user.
errUndeclaredO s = SemanticError 0 $ "Undeclared identifier " ++ s ++ "."
errUndeclaredM s = SemanticError 0 $ "Undeclared method " ++ s ++ "."
errUndeclaredT t = SemanticError 0 $ "Undeclared type " ++ tName t ++ "."
errClazzRedef c = SemanticError 0 $ "Class " ++ cName c ++ " was previously defined."
errClazzUndef t p = SemanticError 0 $ "Class " ++ tName t ++ " inherits from an undefined class " ++ tName p ++ "."
errClazzMain = GenericError $ "Class Main is not defined."
errClazzSelf = SemanticError 0 $ "Class name SELF_TYPE is reserved and cannot be used."
errAttributeOverride s = SemanticError 0 $ "Attribute " ++ s ++ " is an attribute of an inherited class."
errAttributeSelf = SemanticError 0 $ "Attributes cannot have 'self' as their name."
errMethodReturn t u s = SemanticError 0 $ "Inferred return type " ++ tName t ++ " of method " ++ s ++ " does not conform to declared return type " ++ tName u ++ "."
errMethodOverride s = SemanticError 0 $ "Method " ++ s ++ " has incompatible redefinition."
errMethodSelfName = SemanticError 0 $ "Method parameters cannot have 'self' as their name."
errMethodSelfType = SemanticError 0 $ "Method parameter cannot have type SELF_TYPE."
errMethodDupl s = SemanticError 0 $ "Method parameter " ++ s ++ " is multiply defined."
errAssign t u s = SemanticError 0 $ "Type " ++ tName t ++ " of assigned expression does not conform to declared type " ++ tName u ++ " of identifier " ++ s ++ "."
errAssignSelf = SemanticError 0 $ "Illegal assignemnt of 'self' variable."
errDispArg t u s = SemanticError 0 $ "Illegal call of method " ++ s ++ ", type " ++ tName t ++ " of parameter does not conform to declared type " ++ tName u ++ "."
errDispRcvr t u s = SemanticError 0 $ "Illegal call of method " ++ s ++ ", expression type " ++ tName t ++ " does not conform to declared static dispatch type " ++ tName u ++ "."
errConditional = SemanticError 0 $ "Illegal condition on non-bool predicate."
errLoop = SemanticError 0 $ "Illegal loop on non-bool predicate."
errLet t u s = SemanticError 0 $ "Inferred type " ++ tName t ++ " of initialization of " ++ s ++ " does not conform to identifier's declared type " ++ tName u ++ "."
errLetSelf = SemanticError 0 $ "Illegal binding of 'self' in a let expression."
errCaseDupl t = SemanticError 0 $ "Duplicate branch " ++ tName t ++ " in case statement."
errUnaryInt = SemanticError 0 $ "Illegal unary negation on non-integer type."
errUnaryBool = SemanticError 0 $ "Illegal unary not on non-boolean type."
errBinary = SemanticError 0 $ "Illegal binary operation on non-integer types."
errCompareEq = SemanticError 0 $ "Illegal comparison with a basic type."
errCompareRel = SemanticError 0 $ "Illegal comparison of non-integer types."

-- This is the main driving method that analyzes class inheritance and
-- searches for possible class redefinitions overall the whole program.
inheritanceRedefinition :: [Clazz ()] -> Result [Clazz ()]
inheritanceRedefinition [] = return []
inheritanceRedefinition cs = maybe (inheritanceRedefinition css) errClazzRedef (lookupClass css t)
	where (t,css) = (cType $ head cs, tail cs)

-- This is the main driving method that analyzes class inheritance for
-- definiteness overall the whole program.
inheritanceDefiniteness :: [Clazz ()] -> Result [Clazz ()]
inheritanceDefiniteness cs = mapM check cs
	where
		check c@(Clazz _ Nothing _) = return c
		check c@(Clazz t (Just p) _) = maybe (errClazzUndef t p) (const $ return c) (lookupClass cs p)

-- This is the main driving method that searches for the main class.
searchForDefinitionOfMain :: [Clazz ()] -> Result [Clazz ()]
searchForDefinitionOfMain cs = maybe errClazzMain (const $ return cs) (lookupClass cs typeMain)

-- This is the main driving method that performs type checking on the
-- parsed AST. It is one of the main algorithms of semantic analysis.
typecheck :: [Clazz ()] -> Result [Clazz Type]
typecheck cs = do
	mapM attrCheck cs
	mapM methCheck cs
	mapM clazzCheck cs
	where
		clazzCheck c = checkClazz (initEnv c) c
		attrDupes c = dups $ map fst $ initO c
		attrCheck c = unless (null $ attrDupes c) (errAttributeOverride $ head $ attrDupes c)
		methDupes c = dupsBy (\(s1,ts1)(s2,ts2) -> s1 == s2 && ts1 /= ts2) $ map (\((_,s),ts) -> (s,ts)) $ initM' c
		methCheck c = unless (null $ methDupes c) (errMethodOverride . fst $ head $ methDupes c)
		initEnv c = (cType c,initO c,initM,initI)
		initO (Clazz t _ _) = concat $ map initO' (superClasses t)
		initO' (Clazz _ _ fs) = map (\(Attribute s t _) -> (s,t)) (lefts fs)
		initM = concat $ map initM' allClasses
		initM' (Clazz t _ _) = concat $ map (initM'' t) (superClasses t)
		initM'' t (Clazz _ _ fs) = map (\(Method s ps u _) -> ((t,s),map snd ps ++ [u])) (rights fs)
		initI = map (\(Clazz t u _) -> (t,maybe typeObject id u)) allClasses
		superTypes t = takeWhile (/= typeObject) (iterate (\x -> fromJust (lookup x initI)) t) ++ [typeObject]
		superClasses t = map (\x -> fromJust (lookupClass allClasses x)) (superTypes t)
		allClasses = definedClasses no ++ cs

checkClazz :: Environment -> Clazz () -> Result (Clazz Type)
checkClazz env (Clazz t u fs) = do
	fs' <- mapM (either checkAttr checkMeth) fs
	when (t == typeSELF) errClazzSelf
	return (Clazz t u fs')
	where
		checkAttr a = do a' <- checkAttribute env a; return (Left a')
		checkMeth m = do m' <- checkMethod env m; return (Right m')

checkAttribute :: Environment -> Attribute () -> Result (Attribute Type)
checkAttribute env (Attribute s t Nothing) = do
	return (Attribute s t Nothing)
checkAttribute env (Attribute s t (Just e)) = do
	let env' = envPushO [(nameSelf,typeSELF)] env
	e' <- checkExpr env' e
	-- TODO: Subtype checks are still missing!
	when (s == nameSelf) errAttributeSelf
	return (Attribute s t (Just e'))

checkMethod :: Environment -> Method () -> Result (Method Type)
checkMethod env (Method s ps t e) = do
	let env' = envPushO ((nameSelf,typeSELF):ps) env
	e' <- checkExpr env' e
	let t' = annotation e'
	unless (isKnownType env t) (errUndeclaredT t)
	unless (isSubtype env t' t) (errMethodReturn t' t s)
	when (any (== nameSelf) (map fst ps)) errMethodSelfName
	when (any (== typeSELF) (map snd ps)) errMethodSelfType
	unless (null $ dups $ map fst ps) (errMethodDupl $ head $ dups $ map fst ps)
	return (Method s ps t e')

checkExpr :: Environment -> Expression () -> Result (Expression Type)
checkExpr env (ConstantExpr _ tok@(Boolean _)) = do
	return (ConstantExpr typeBool tok)
checkExpr env (ConstantExpr _ tok@(Integer _ _)) = do
	return (ConstantExpr typeInt tok)
checkExpr env (ConstantExpr _ tok@(String _)) = do
	return (ConstantExpr typeString tok)
checkExpr env (IdentifierExpr _ s) = do
	t <- envO env s
	return (IdentifierExpr t s)
checkExpr env (AssignmentExpr _ s e) = do
	t <- envO env s
	e' <- checkExpr env e
	let t' = annotation e'
	unless (isSubtype env t' t) (errAssign t' t s)
	when (s == nameSelf) errAssignSelf
	return (AssignmentExpr t' s e')
checkExpr env (DispatchExpr _ e t s es) = do
	e' <- checkExpr env e
	let t0 = annotation e'
	let t0' = fixType env t0
	let t' = maybe t0' id t
	ts' <- envM env t' s
	es' <- mapM checkArgument (zip es $ init ts')
	let tn' = last ts'
	let tn = if (tn' == typeSELF) then t0 else tn'
	unless (isSubtype env t0 t') (errDispRcvr t0 t' s)
	return (DispatchExpr tn e' t s es')
	where checkArgument (e,t) = do
		e' <- checkExpr env e
		let t' = annotation e'
		unless (isSubtype env t' t) (errDispArg t' t s)
		return e'
checkExpr env (ConditionalExpr _ e1 e2 e3) = do
	e1' <- checkExpr env e1
	e2' <- checkExpr env e2
	e3' <- checkExpr env e3
	unless (annotation e1' == typeBool) errConditional
	let t = joinType env (annotation e2') (annotation e3')
	return (ConditionalExpr t e1' e2' e3')
checkExpr env (LoopExpr _ e1 e2) = do
	e1' <- checkExpr env e1
	e2' <- checkExpr env e2
	unless (annotation e1' == typeBool) errLoop
	return (LoopExpr typeObject e1' e2')
checkExpr env (BlockExpr _ es) = do
	es' <- mapM (checkExpr env) es
	let t = annotation $ last es'
	return (BlockExpr t es')
checkExpr env (LetExpr _ s t e1 e2) = do
	let env' = envPushO [(s,t)] env
	e1' <- maybe (return Nothing) checkInit e1
	e2' <- checkExpr env' e2
	let t2 = annotation e2'
	when (s == nameSelf) errLetSelf
	return (LetExpr t2 s t e1' e2')
	where checkInit e = do
		e' <- checkExpr env e
		let t' = annotation e'
		unless (isSubtype env t' t) (errLet t' t s)
		return (Just e')
checkExpr env (CaseExpr _ e cs) = do
	e' <- checkExpr env e
	cs' <- mapM checkCase cs
	unless (null $ dups $ map scnd cs) (errCaseDupl $ head $ dups $ map scnd cs)
	let t = foldl1 (joinType env) (map (annotation . thrd) cs')
	return (CaseExpr t e' cs')
	where checkCase (s,t,e) = do
		let env' = envPushO [(s,t)] env
		e' <- checkExpr env' e
		return (s,t,e')
checkExpr env (NewExpr _ t) = do
	unless (isKnownType env t) (errUndeclaredT t)
	return (NewExpr t t)
checkExpr env (IsVoidExpr _ e) = do
	e' <- checkExpr env e
	return (IsVoidExpr typeBool e')
checkExpr env (UnaryOpExpr _ IntNegOp e) = do
	e' <- checkExpr env e
	unless (annotation e' == typeInt) errUnaryInt
	return (UnaryOpExpr typeInt IntNegOp e')
checkExpr env (UnaryOpExpr _ BoolNegOp e) = do
	e' <- checkExpr env e
	unless (annotation e' == typeBool) errUnaryBool
	return (UnaryOpExpr typeBool BoolNegOp e')
checkExpr env (BinaryOpExpr _ op e1 e2) = do
	e1' <- checkExpr env e1
	e2' <- checkExpr env e2
	unless (annotation e1' == typeInt) errBinary
	unless (annotation e2' == typeInt) errBinary
	return (BinaryOpExpr typeInt op e1' e2')
checkExpr env (ComparisonOpExpr _ EqualOp e1 e2) = do
	e1' <- checkExpr env e1
	e2' <- checkExpr env e2
	let t1 = annotation e1'
	let t2 = annotation e2'
	let ts = [typeInt,typeString,typeBool]
	when (t1 `elem` ts && t1 /= t2) errCompareEq
	when (t2 `elem` ts && t1 /= t2) errCompareEq
	return (ComparisonOpExpr typeBool EqualOp e1' e2')
checkExpr env (ComparisonOpExpr _ op e1 e2) = do
	e1' <- checkExpr env e1
	e2' <- checkExpr env e2
	unless (annotation e1' == typeInt) errCompareRel
	unless (annotation e2' == typeInt) errCompareRel
	return (ComparisonOpExpr typeBool op e1' e2')

-- Helper method to convert an abstract into a concrete type.
fixType :: Environment -> Type -> Type
fixType env t = if (t == typeSELF) then envC env else t

-- Helper to determine whether a type is known in the program.
isKnownType :: Environment -> Type -> Bool
isKnownType env t1 = let t1super = envI env t1' in
	t1' == typeObject || isJust t1super
	where t1' = fixType env t1

-- Helper to check a subtype relationship.
isSubtype :: Environment -> Type -> Type -> Bool
isSubtype env t1 t2 = let t1super = envI env t1' in
	t1 == t2 || t1' == t2 || t1' /= typeObject && isSubtype env (fromJust t1super) t2
	where t1' = fixType env t1

-- Helper to find the smallest common supertype.
joinType :: Environment -> Type -> Type -> Type
joinType env t1 t2 = let t1super = envI env t1' in
	if isSubtype env t2 t1' then t1 else joinType env (fromJust t1super) t2
	where t1' = fixType env t1

-- Helper method to find duplicates in a list of elements.
dups :: (Eq a,Ord a) => [a] -> [a]
dups xs = map head $ filter (\x -> length x > 1) (group $ sort xs)

-- Helper method to find duplicates in a list of elements.
dupsBy :: (Eq a,Ord a) => (a -> a -> Bool) -> [a] -> [a]
dupsBy f xs = map head $ filter (\x -> length x > 1) (groupBy f $ sort xs)

-- Helper to retrieve the second element in a triplet.
scnd :: (a,b,c) -> b
scnd (_,x,_) = x

-- Helper to retrieve the third element in a triplet.
thrd :: (a,b,c) -> c
thrd (_,_,x) = x

-- Helper to look for a class definition in the program.
lookupClass :: [Clazz a] -> Type -> Maybe (Clazz a)
lookupClass cs t = find (\c -> cType c == t) cs

-- This is the main method specifing all steps required to run through
-- semantic analysis. Note that type checking is the step that will
-- actually add type annotations to the AST.
semantPhases :: [Clazz ()] -> Result [Clazz Type]
semantPhases cs = do
	inheritanceRedefinition (definedClasses no ++ cs)
	inheritanceDefiniteness (inheritableClasses no ++ cs)
	searchForDefinitionOfMain cs
	typecheck cs

-- This is the main entry point for the semantic analysis.
semant :: String -> Result [Clazz Type]
semant s = case CoolParser.parse s of
	(CoolParser.Program p)       -> semantPhases p
	(CoolParser.LexerError l m)  -> LexerError l m
	(CoolParser.ParserError l t) -> ParserError l (show t)
