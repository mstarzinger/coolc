-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolPrinter (
		prettyProgram,
	) where
import CoolAST
import CoolLexer

class ShowableAnnotation a where
	showAnnotation :: a -> String

instance ShowableAnnotation () where
	showAnnotation _ = "_no_type"

instance ShowableAnnotation Type where
	showAnnotation = tName

prettyProgram :: (ShowableAnnotation a) => [Clazz a] -> IO ()
prettyProgram cs = do
	p 0 "#0"
	p 0 "_program"
	mapM_ prettyClazz cs

prettyClazz :: (ShowableAnnotation a) => Clazz a -> IO ()
prettyClazz (Clazz t u fs) = do
	p 1 "#0"
	p 1 "_class"
	p 2 $ tName t
	p 2 $ maybe "Object" tName u
	p 2 "filename"
	p 2 "("
	mapM_ (either prettyAttribute prettyMethod) fs
	p 2 ")"

prettyAttribute :: (ShowableAnnotation a) => Attribute a -> IO ()
prettyAttribute (Attribute s t e) = do
	p 2 "#0"
	p 2 "_attr"
	p 3 s
	p 3 $ tName t
	prettyMaybeExpr 3 e

prettyMethod :: (ShowableAnnotation a) => Method a -> IO ()
prettyMethod (Method s ps t e) = do
	p 2 "#0"
	p 2 "_method"
	p 3 s
	mapM_ pParameter ps
	p 3 $ tName t
	prettyExpr 3 e
	where pParameter (s,t) = do
		p 3 "#0"
		p 3 "_formal"
		p 4 s
		p 4 $ tName t

prettyExpr :: (ShowableAnnotation a) => Int -> Expression a -> IO ()
prettyExpr n e = do
	p n "#0"
	prettyExpr2 n e
	p n $ ": " ++ showAnnotation (annotation e)

prettyMaybeExpr :: (ShowableAnnotation a) => Int -> Maybe (Expression a) -> IO ()
prettyMaybeExpr n (Just e) = prettyExpr n e
prettyMaybeExpr n Nothing = do
	p n "#0"
	p n "_no_expr"
	p n ": _no_type"

prettyExpr2 :: (ShowableAnnotation a) => Int -> Expression a -> IO ()
prettyExpr2 n (ConstantExpr _ (Boolean b)) = do
	p n "_bool"
	p (n+1) $ if b then "1" else "0"
prettyExpr2 n (ConstantExpr _ (Integer _ i)) = do
	p n "_int"
	p (n+1) $ show i
prettyExpr2 n (ConstantExpr _ (String s)) = do
	p n "_string"
	p (n+1) $ show s
prettyExpr2 n (IdentifierExpr _ s) = do
	p n "_object"
	p (n+1) s
prettyExpr2 n (AssignmentExpr _ s e) = do
	p n "_assign"
	p (n+1) s
	prettyExpr (n+1) e
prettyExpr2 n (DispatchExpr _ e Nothing s es) = do
	p n "_dispatch"
	prettyExpr (n+1) e
	p (n+1) s
	p (n+1) "("
	mapM_ (prettyExpr (n+1)) es
	p (n+1) ")"
prettyExpr2 n (DispatchExpr _ e (Just t) s es) = do
	p n "_static_dispatch"
	prettyExpr (n+1) e
	p (n+1) $ tName t
	p (n+1) s
	p (n+1) "("
	mapM_ (prettyExpr (n+1)) es
	p (n+1) ")"
prettyExpr2 n (ConditionalExpr _ e1 e2 e3) = do
	p n "_cond"
	prettyExpr (n+1) e1
	prettyExpr (n+1) e2
	prettyExpr (n+1) e3
prettyExpr2 n (LoopExpr _ e1 e2) = do
	p n "_loop"
	prettyExpr (n+1) e1
	prettyExpr (n+1) e2
prettyExpr2 n (BlockExpr _ es) = do
	p n "_block"
	mapM_ (prettyExpr (n+1)) es
prettyExpr2 n (LetExpr _ s t e1 e2) = do
	p n "_let"
	p (n+1) s
	p (n+1) $ tName t
	prettyMaybeExpr (n+1) e1
	prettyExpr (n+1) e2
prettyExpr2 n (CaseExpr _ e cs) = do
	p n "_typcase"
	prettyExpr (n+1) e
	mapM_ pCase cs
	where pCase (s,t,e) = do
		p (n+1) "#0"
		p (n+1) "_branch"
		p (n+2) s
		p (n+2) $ tName t
		prettyExpr (n+2) e
prettyExpr2 n (NewExpr _ t) = do
	p n "_new"
	p (n+1) $ tName t
prettyExpr2 n (IsVoidExpr _ e) = do
	p n "_isvoid"
	prettyExpr (n+1) e
prettyExpr2 n (UnaryOpExpr _ op e) = do
	p n $ showOp op
	prettyExpr (n+1) e
	where
		showOp IntNegOp = "_neg"
		showOp BoolNegOp = "_comp"
prettyExpr2 n (BinaryOpExpr _ op e1 e2) = do
	p n $ showOp op
	prettyExpr (n+1) e1
	prettyExpr (n+1) e2
	where
		showOp AddOp = "_plus"
		showOp SubOp = "_sub"
		showOp MulOp = "_mul"
		showOp DivOp = "_divide"
prettyExpr2 n (ComparisonOpExpr _ op e1 e2) = do
	p n $ showOp op
	prettyExpr (n+1) e1
	prettyExpr (n+1) e2
	where
		showOp LessOp = "_lt"
		showOp LessOrEqualOp = "_leq"
		showOp EqualOp = "_eq"

p :: Int -> String -> IO ()
p n s = do
	putStr $ replicate (2 * n) ' ' ++ s ++ "\n"
