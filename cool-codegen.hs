-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolCodegen (
		generate
	) where
import Assembler
import Assembler.MIPS
import CoolAST
import CoolBasic
import CoolLexer
import CoolSemant (lookupClass)
import Control.Monad (unless,when)
import Data.Char (ord)
import Data.Either (lefts,rights)
import Data.List (elemIndices,intersperse,nubBy)
import Data.Maybe (fromJust,isJust,isNothing)
import Data.Unique (hashUnique,newUnique)

-- The data type representing the result of code generation. Can be used
-- as a monad with the provided bind and return operations.
data Result a = Result {mnd :: a, asm :: MIPSAssembly}
instance Monad Result where
	(>>=) m k = Result (mnd n) (asm m ++ asm n) where n = k (mnd m)
	return x = Result x []

-- Helper to append a single directive to the current result monad.
__ :: MIPSDirective -> Result ()
__ d = Result () [d]

-- Generates a mangled label based on a type and an arbitrary string.
mangleClazz :: Type -> String -> String
mangleClazz t s = "_cool_" ++ mangledT ++ "_" ++ s
	where
		mangledT = (show $ length $ tName t) ++ tName t

-- Generates a mangled label based on a type and a method name.
mangleMethod :: Type -> String -> String
mangleMethod t m = "_cool_" ++ mangledT ++ "_" ++ mangledM
	where
		mangledT = (show $ length $ tName t) ++ tName t
		mangledM = (show $ length m) ++ m

-- Generates a unique label based on the current label counter.
uniqueLabel :: Type -> String -> Integer -> String -> String
uniqueLabel t m i s = "_cool_" ++ mangledT ++ "_" ++ mangledM ++ "_" ++ (show i) ++ "_" ++ s
	where
		mangledT = (show $ length $ tName t) ++ tName t
		mangledM = (show $ length m) ++ m

-- The size of a pointer on the target architecture.
sizeOfPointer :: Integer
sizeOfPointer = 4

-- ---------------------------------------------------------------------
-- Static class information is stored in the data segment of the program
-- so that it is available at runtime. This contains meta-information
-- about the class (e.g. class name and instance size) as well as the
-- virtual function table that is used to dispatch method calls. The
-- following layout is used for objects and class information.
--
--    +-----------+
--    |  Clazz ---+------>+-----------------+
--    |-----------|       |  Class Name ----+------> ASCIIZ String
--    |  Field 1  |       |-----------------|
--    |   . . .   |       |  Instance Size  |
--    |  Field n  |       |-----------------|
--    +-----------+       |  Super Clazz ---+------> Class Info
--                        |-----------------|
--                        |  Initializer ---+------> Function Entry
--                        |-----------------|
--                        |  Method Ptr 1 --+------> Function Entry
--                        |     . . .       |            . . .
--                        |  Method Ptr m --+------> Function Entry
--                        +-----------------+
--
-- In the above example the object has n fields and m methods plus one
-- initializer. The fields are stored in object instances whereas the
-- dispatch table and meta-information is shared among all instanzes.
--
-- TODO: We preserve the class hirarchy and store a pointer to the super
-- class in order to perform instance checks in case statements. This
-- could also be done by ensuring class information objects are stored
-- in a depth first traversal order of the inheritance graph. This would
-- in turn allow us to realize instance checks as simple pointer range
-- comparisons.
-- ---------------------------------------------------------------------
objectClazzOffset = 0 * sizeOfPointer;
objectFieldOffset = 1 * sizeOfPointer;
clazzNameOffset = 0 * sizeOfPointer;
clazzSizeOffset = 1 * sizeOfPointer;
clazzPrntOffset = 2 * sizeOfPointer;
clazzInitOffset = 3 * sizeOfPointer;
clazzVTblOffset = 4 * sizeOfPointer;

-- ---------------------------------------------------------------------
-- We employ a non-standard procedure call convention (aka. ABI) which
-- aims at a simple implementation and might sacrifice performance. All
-- arguments are passed via the stack. We use the dedicated $v1 register
-- to hold the 'self' variable. The return value is passed via $v0 as
-- usual. The following is rough layout of an activation record (aka.
-- stack frame), where the registers $sp, $fp and $ra have canonical
-- semantics.
--
--            +--------------+   higher memory addresses
--            |    . . .     |
--            |  Argument 2  |
--            |  Argument 1  |
--            |--------------+
--    $fp --> |  Saved $fp   |
--            |--------------|
--            |  Saved $ra   |
--            |--------------|
--            |  Local Vars  |
--            |    . . .     |
--    $sp --> +--------------+   lower memory addresses
--
-- Note that we don't care about alignment of the stack frame because
-- all our values will be pointer sized only.
-- ---------------------------------------------------------------------
frameFPOffset = 0 * sizeOfPointer;
frameRAOffset = 1 * sizeOfPointer;
frameSize = 2 * sizeOfPointer;

-- ---------------------------------------------------------------------
-- We represent values of primitive types (i.e. booleans, integers and
-- strings) in an optimized form that allows us to directly use machine
-- instructions when dealing with them. However the type system allows
-- these primitive types to be treated as objects. In these cases we
-- perform automative boxing to wrap these values into an object.
--
-- Boolean: The false value is represented as 0, the true value as 1.
-- Integer: The binary representation of the integral value as int32.
-- String: The pointer to a zero-terminated ASCII string in memory or
--         the void pointer to represent an empty string.
-- ---------------------------------------------------------------------
boxingValueOffset = objectFieldOffset
boxingObjectSize = 2 * sizeOfPointer

-- Emits code for the actual program.
emitGeneratedCode :: [Clazz Type] -> Result ()
emitGeneratedCode cs = do
	mapM_ emitInfo allClasses
	mapM_ emitInit allClasses
	mapM_ emitCode cs
	where
		emitCode c = simpleClazz (initEnv c) c
		emitInfo c = simpleInfo (initEnv c) c
		emitInit c = simpleInit (initEnv c) c
		initEnv c = (cType c,0,initS,initV c,initM)
		initS = map (\c -> (cType c,toInteger (length (initV c)) * sizeOfPointer + objectFieldOffset)) allClasses
		initV (Clazz t _ _) = zipWith (\(a,b) c -> (a,(c,b))) (concat $ map initV' (superClasses t)) (map ObjectLocation [0..])
		initV' (Clazz _ _ fs) = map (\(Attribute s t _) -> (s,t)) (lefts fs)
		initM =  concat $ map initM' allClasses
		initM' (Clazz t _ _) = zipWith (\(a,b) c -> (a,(c,b))) (resolveOverrides $ concat $ map (initM'' t) (superClasses t)) [0..]
		initM'' t (Clazz u _ fs) = map (\(Method s _ _ _) -> ((t,s),u)) (rights fs)
		initI = map (\(Clazz t u _) -> (t,maybe typeObject id u)) allClasses
		superTypes t = reverse $ takeWhile (/= typeObject) (iterate (\x -> fromJust (lookup x initI)) t) ++ [typeObject]
		superClasses t = map (\x -> fromJust (lookupClass allClasses x)) (superTypes t)
		allClasses = definedClasses typeObject ++ cs
		resolveOverrides [] = []
		resolveOverrides xs = (xs !! last (elemIndices (head xs') xs')) : (resolveOverrides (tail xs)) where xs' = map fst xs

-- This is the main method specifing all steps required to run through
-- code generation. The resulting monad will contain the assembly code.
runGenerator :: [Clazz Type] -> Result ()
runGenerator cs = do
	emitGeneratedCode cs
	emitRuntimeAbort
	emitRuntimeTypeName
	emitRuntimeCopy
	emitRuntimeOutputString
	emitRuntimeOutputInt
	emitRuntimeInputString
	emitRuntimeInputInt
	emitRuntimeLength
	emitRuntimeConcat
	emitRuntimeSubstr
	emitBuiltinStringEquals
	emitErrorHandlers
	emitEntryTrampoline

-- This is the main entry point for the code generation.
generate :: [Clazz Type] -> MIPSAssembly
generate = asm . runGenerator

-- ---------------------------------------------------------------------
-- This part of the file has code generators for the runtime functions
-- that are provided by the basic language classes.
-- ---------------------------------------------------------------------

-- Emits an entry point that kicks us into generated code.
emitEntryTrampoline :: Result ()
emitEntryTrampoline = do
	__$ cmt "Main entry point into the program."
	__$ lbl "main"
	__$ la a0 (mangleClazz typeMain "clazz")	-- load main class
	__$ lw a1 a0 clazzSizeOffset				-- load instance size
	allocateDynamic a1 v1						-- allocate new instance
	__$ sw a0 v1 objectClazzOffset				-- set object class
	__$ jal (mangleMethod typeMain nameInit)	-- jump to main initializer
	__$ jal (mangleMethod typeMain nameMain)	-- jump to main method
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_main"						-- load string address
	__$ syscall
	__$ li v0 10								-- syscall 10 (exit)
	__$ syscall
	__$ dir ".data"
	__$ lbl "_cool_main"
	__$ dir ".asciiz \"COOL program successfully executed\\n\""
	__$ dir ".text"

-- Emits error handlers for several runtime errors.
emitErrorHandlers :: Result ()
emitErrorHandlers = do
	__$ cmt "Error handlers for several runtime errors."
	__$ lbl "_cool_error_void_dispatch"
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_error_1"					-- load string address
	__$ syscall
	__$ li v0 10								-- syscall 10 (exit)
	__$ syscall
	__$ lbl "_cool_error_void_case"
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_error_2"					-- load string address
	__$ syscall
	__$ li v0 10								-- syscall 10 (exit)
	__$ syscall
	__$ lbl "_cool_error_unmatched_case"
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_error_3"					-- load string address
	__$ syscall
	__$ li v0 4									-- syscall 4 (print_str)
	__$ lw a0 v1 objectClazzOffset				-- load object class
	__$ lw a0 a0 clazzNameOffset				-- load class name
	__$ syscall
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_error_4"					-- load string address
	__$ syscall
	__$ li v0 10								-- syscall 10 (exit)
	__$ syscall
	__$ dir ".data"
	__$ lbl "_cool_error_1"
	__$ dir ".asciiz \"filename.cl:0: Dispatch to void.\\n\""
	__$ lbl "_cool_error_2"
	__$ dir ".asciiz \"Match on void in case statement.\\n\""
	__$ lbl "_cool_error_3"
	__$ dir ".asciiz \"No match in case statement for Class \""
	__$ lbl "_cool_error_4"
	__$ dir ".asciiz \"\\n\""
	__$ dir ".text"

-- Emits implementation of abort() runtime method.
emitRuntimeAbort :: Result ()
emitRuntimeAbort = do
	__$ cmt "Implementation of abort() runtime method."
	__$ lbl (mangleMethod typeObject nameAbort)
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_abort_1"					-- load string address
	__$ syscall
	__$ li v0 4									-- syscall 4 (print_str)
	__$ lw a0 v1 objectClazzOffset				-- load object class
	__$ lw a0 a0 clazzNameOffset				-- load class name
	__$ syscall
	__$ li v0 4									-- syscall 4 (print_str)
	__$ la a0 "_cool_abort_2"					-- load string address
	__$ syscall
	__$ li v0 10								-- syscall 10 (exit)
	__$ syscall
	__$ dir ".data"
	__$ lbl "_cool_abort_1"
	__$ dir ".asciiz \"Abort called from class \""
	__$ lbl "_cool_abort_2"
	__$ dir ".asciiz \"\\n\""
	__$ dir ".text"

-- Emits implementation of type_name() runtime method.
emitRuntimeTypeName :: Result ()
emitRuntimeTypeName = do
	__$ cmt "Implementation of type_name() runtime method."
	__$ lbl (mangleMethod typeObject nameTypeName)
	__$ lw a0 v1 objectClazzOffset				-- load object class
	__$ lw v0 a0 clazzNameOffset				-- load class name
	__$ jr ra									-- return to caller

-- Emits implementation of copy() runtime method.
emitRuntimeCopy :: Result ()
emitRuntimeCopy = do
	__$ cmt "Implementation of copy() runtime method."
	__$ lbl (mangleMethod typeObject nameCopy)
	__$ lw a0 v1 objectClazzOffset				-- load object class
	__$ la a1 (mangleClazz typeInt "clazz")		-- check for integer type
	__$ beq a0 a1 "_cool_copy2"					-- is immutable, no copy
	__$ la a1 (mangleClazz typeBool "clazz")	-- check for boolean type
	__$ beq a0 a1 "_cool_copy2"					-- is immutable, no copy
	__$ la a1 (mangleClazz typeString "clazz")	-- check for string type
	__$ beq a0 a1 "_cool_copy2"					-- is immutable, no copy
	__$ lw a2 a0 clazzSizeOffset				-- load instance size
	allocateDynamic a2 v0						-- allocate new instance
	__$ move a0 v1
	__$ move a1 v0
	__$ lbl "_cool_copy1"
	__$ lw a3 a0 0								-- load word from source
	__$ sw a3 a1 0								-- store word in destination
	__$ addiu a0 a0 sizeOfPointer				-- bump source pointer
	__$ addiu a1 a1 sizeOfPointer				-- bump destination pointer
	__$ addiu a2 a2 (-sizeOfPointer)			-- decrement copy count
	__$ bne a2 zero "_cool_copy1"				-- repeat until happy
	__$ jr ra									-- return to caller
	__$ lbl "_cool_copy2"
	__$ move v0 v1								-- just return receiver
	__$ jr ra									-- return to caller

-- Emits implementation of out_string() runtime method.
emitRuntimeOutputString :: Result ()
emitRuntimeOutputString = do
	__$ cmt "Implementation of out_string() runtime method."
	__$ lbl (mangleMethod typeIO nameOutputString)
	__$ lw a0 sp (1 * sizeOfPointer)			-- print first argument
	__$ beqz a0 "_cool_out_string"
	__$ li v0 4									-- syscall 4 (print_str)
	__$ syscall
	__$ lbl "_cool_out_string"
	__$ jr ra									-- return to caller

-- Emits implementation of out_int() runtime method.
emitRuntimeOutputInt :: Result ()
emitRuntimeOutputInt = do
	__$ cmt "Implementation of out_int() runtime method."
	__$ lbl (mangleMethod typeIO nameOutputInt)
	__$ lw a0 sp (1 * sizeOfPointer)			-- print first argument
	__$ li v0 1									-- syscall 1 (print_int)
	__$ syscall
	__$ jr ra									-- return to caller

-- Emits implementation of in_string() runtime method.
emitRuntimeInputString :: Result ()
emitRuntimeInputString = do
	__$ cmt "Implementation of in_string() runtime method."
	__$ lbl (mangleMethod typeIO nameInputString)
	-- TODO: Input of strings not yet implemented!
	__$ j (mangleMethod typeObject nameAbort)

-- Emits implementation of in_int() runtime method.
emitRuntimeInputInt :: Result ()
emitRuntimeInputInt = do
	__$ cmt "Implementation of in_int() runtime method."
	__$ lbl (mangleMethod typeIO nameInputInt)
	-- TODO: Input of integers not yet implemented!
	__$ j (mangleMethod typeObject nameAbort)

-- Emits implementation of length() runtime method.
emitRuntimeLength :: Result ()
emitRuntimeLength = do
	__$ cmt "Implementation of length() runtime method."
	__$ lbl (mangleMethod typeString nameLength)
	__$ move v0 zero							-- initialize counter
	__$ lw a0 v1 boxingValueOffset				-- unbox receiver value
	__$ beqz a0 "_cool_length_2"				-- check for empty string
	__$ lbl "_cool_length_1"
	__$ lb a1 a0 0								-- load character
	__$ beqz a1 "_cool_length_2"				-- check for terminator
	__$ addiu v0 v0 1							-- increment counter
	__$ addiu a0 a0 1							-- bump string pointer
	__$ j "_cool_length_1"						-- repeat until happy
	__$ lbl "_cool_length_2"
	__$ jr ra									-- return to caller

-- Emits implementation of concat() runtime method.
emitRuntimeConcat :: Result ()
emitRuntimeConcat = do
	__$ cmt "Implementation of concat() runtime method."
	__$ lbl (mangleMethod typeString nameConcat)
	__$ lw a0 v1 boxingValueOffset				-- unbox receiver value
	__$ lw a1 sp sizeOfPointer					-- load first argument
	__$ beqz a0 "_cool_concat_3"				-- check for empty string
	__$ beqz a1 "_cool_concat_4"				-- check for empty string
	__$ move v0 gp								-- result will be a new string
	__$ lbl "_cool_concat_1"					-- copying loop of 1st string
	__$ lb a2 a0 0								-- load character
	__$ beqz a2 "_cool_concat_2"				-- check for terminator
	__$ sb a2 gp 0								-- store character
	__$ addiu a0 a0 1							-- bump 1st string pointer
	__$ addiu gp gp 1							-- bump allocation pointer
	__$ j "_cool_concat_1"
	__$ lbl "_cool_concat_2"					-- copying loop of 2nd string
	__$ lb a2 a1 0								-- load character
	__$ sb a2 gp 0								-- store character
	__$ addiu a1 a1 1							-- bump 2nd string pointer
	__$ addiu gp gp 1							-- bump allocation pointer
	__$ bne a2 zero "_cool_concat_2"			-- check for terminator
	__$ addiu gp gp (sizeOfPointer-1)			-- align allocation pointer
	__$ li a2 (-sizeOfPointer)
	__$ and_ gp gp a2
	__$ jr ra									-- return to caller
	__$ lbl "_cool_concat_3"
	__$ move v0 a1								-- just return first argument
	__$ jr ra									-- return to caller
	__$ lbl "_cool_concat_4"
	__$ move v0 a0								-- just return receiver
	__$ jr ra									-- return to caller

-- Emits implementation of substr() runtime method.
emitRuntimeSubstr :: Result ()
emitRuntimeSubstr = do
	__$ cmt "Implementation of substr() runtime method."
	__$ lbl (mangleMethod typeString nameSubstr)
	__$ lw a0 v1 boxingValueOffset				-- unbox receiver value
	__$ beqz a0 "_cool_substr_5"				-- check for empty string
	__$ lw a1 sp (1*sizeOfPointer)				-- load first argument
	__$ lw a2 sp (2*sizeOfPointer)				-- load second argument
	__$ move v0 gp								-- result will be a new string
	__$ lbl "_cool_substr_1"					-- skipping loop for string
	__$ beqz a1 "_cool_substr_2"				-- check if skipping done
	__$ lb a3 a0 0								-- load character
	__$ beqz a3 "_cool_substr_5"				-- check for terminator
	__$ addiu a0 a0 1							-- bump string pointer
	__$ addiu a1 a1 (-1)						-- decrement skip count
	__$ j "_cool_substr_1"
	__$ lbl "_cool_substr_2"					-- copying loop for string
	__$ beqz a2 "_cool_substr_3"				-- check if copying done
	__$ lb a3 a0 0								-- load character
	__$ sb a3 gp 0								-- store character
	__$ addiu a0 a0 1							-- bump string pointer
	__$ addiu gp gp 1							-- bump allocation pointer
	__$ beqz a2 "_cool_concat_4"				-- check for terminator
	__$ addiu a2 a2 (-1)						-- decrement copy count
	__$ j "_cool_substr_2"
	__$ lbl "_cool_substr_3"
	__$ sb zero gp 0							-- store terminator
	__$ addiu gp gp 1							-- bump allocation pointer
	__$ lbl "_cool_substr_4"
	__$ addiu gp gp (sizeOfPointer-1)			-- align allocation pointer
	__$ li a2 (-sizeOfPointer)
	__$ and_ gp gp a2
	__$ jr ra									-- return to caller
	__$ lbl "_cool_substr_5"
	__$ move v0 zero							-- just return empty string
	__$ jr ra									-- return to caller

-- Emits implementation of string equality builtin.
emitBuiltinStringEquals :: Result ()
emitBuiltinStringEquals = do
	__$ cmt "Implementation of string equality builtin."
	__$ lbl "_streq"
	__$ beq v0 a0 "_streq_true"
	__$ beqz v0 "_streq_false"
	__$ beqz a0 "_streq_false"
	__$ lbl "_streq_loop"
	__$ lb a1 v0 0
	__$ lb a2 a0 0
	__$ bne a1 a2 "_streq_false"
	__$ beqz a1 "_streq_true"
	__$ addiu v0 v0 1
	__$ addiu a0 a0 1
	__$ j "_streq_loop"
	__$ lbl "_streq_true"
	__$ li v0 1
	__$ jr ra
	__$ lbl "_streq_false"
	__$ li v0 0
	__$ jr ra

-- ---------------------------------------------------------------------
-- This part of the file holds macro assembler helpers that produce some
-- common code sequences that are used by the code generator.
-- ---------------------------------------------------------------------

-- Helper to generate automative boxing sequence if necessary.
autoBox :: Type -> Type -> Result ()
autoBox t u
	| (t `elem` [typeInt,typeBool,typeString] && u == typeObject) = do
		allocateStatic boxingObjectSize a0		-- allocate new instance
		__$ la a1 (mangleClazz t "clazz")		-- load class info address
		__$ sw a1 a0 objectClazzOffset			-- set object class
		__$ sw v0 a0 boxingValueOffset			-- store value in box
		__$ move v0 a0							-- switch to the boxed value
	| otherwise = return ()

-- Helper to generate automative unboxing sequence if necessary.
autoUnbox :: Type -> Type -> Result ()
autoUnbox u t
	| (t `elem` [typeInt,typeBool,typeString] && u == typeSELF) = do
		__$ lw v0 v0 boxingValueOffset			-- load value from box
	| otherwise = return ()

-- Helper to generate allocation sequence for static size.
allocateStatic :: Integer -> Register -> Result ()
allocateStatic size rd = do
	__$ move rd gp								-- result will be new object
	__$ addiu gp gp size						-- bump allocation pointer

-- Helper to generate allocation sequence for dynamic size.
allocateDynamic :: Register -> Register -> Result ()
allocateDynamic rsize rd = do
	__$ move rd gp								-- result will be new object
	__$ add gp gp rsize							-- bump allocation pointer

-- ---------------------------------------------------------------------
-- This part of the file defines the two main data structure (i.e. State
-- and Environment) that the code generator uses.
-- ---------------------------------------------------------------------

-- The code generator state represents a mutable state that is threaded
-- through all steps while generating code for an expression. It carries
-- along a label counter for generating unique lables as well as a pool
-- of all accumulated string constants.
type State = (LabelCounter,String,ConstantPool)
type LabelCounter = Integer
type ConstantPool = [(String,String)]

stateInit :: String -> State
stateInit m = (1,m,[])

stateL :: State -> LabelCounter
stateL (l,_,_) = l

stateM :: State -> String
stateM (_,m,_) = m

stateC :: State -> ConstantPool
stateC (_,_,c) = c

statePushL :: LabelCounter -> State -> State
statePushL l1 (l2,m,c) = (l1+l2,m,c)

statePushC :: (String,String) -> State -> State
statePushC c1 (l,m,c2) = (l,m,c1:c2)

-- The code generator environment represents the context under which an
-- expression is being evaluated. It contains declaring class type, the
-- current depth of the machine stack, a mapping of instance sizes, the
-- variable bindings as well as all method bindings that are lexically
-- visible to the expression.
type Environment = (Type,Integer,InstanceSizeEnv,VariableEnv,MethodEnv)
type InstanceSizeEnv = [(Type,Integer)]
type VariableEnv = [(String,(Location,Type))]
type MethodEnv = [((Type,String),(Integer,Type))]

-- The data type representing the location of a variable binding. This
-- reflects the location where the value of a bound variable will be
-- stored at runtime.
data Location =
	ObjectLocation Integer		|	-- Variable bound to an object slot.
	ArgumentLocation Integer	|	-- Variable bound to an argument slot.
	StackLocation Integer		|	-- Variable bound to a stack slot.
	SelfLocation					-- Variable is the 'self' variable.
		deriving Show

envC :: Environment -> Type
envC (c,_,_,_,_) = c

envD :: Environment -> Integer
envD (_,d,_,_,_) = d

envS :: Environment -> Type -> Integer
envS (_,_,s,_,_) t = fromJust (lookup t s)

envV :: Environment -> String -> (Location,Type)
envV (_,_,_,v,_) s = fromJust (lookup s v)

envM :: Environment -> Type -> String -> (Integer,Type)
envM (_,_,_,_,m) t s = fromJust (lookup (t,s) m)

envMs :: Environment -> Type -> [(String,Integer,Type)]
envMs (_,_,_,_,m) t = map (\((_,a),(b,c)) -> (a,b,c)) $ filter (\((x,_),_) -> x == t) m

envPushD :: Environment -> Environment
envPushD (c,d,s,v,m) = (c,d+1,s,v,m)

envPushV :: VariableEnv -> Environment -> Environment
envPushV v1 (c,d,s,v2,m) = (c,d,s,v1++v2,m)

-- ---------------------------------------------------------------------
-- This part of the file implements a simple code generator based on a
-- one-register stack machine which uses a plain operand stack.
-- ---------------------------------------------------------------------

simpleClazz :: Environment -> Clazz Type -> Result ()
simpleClazz env (Clazz t u fs) = do
	mapM_ (simpleMethod env) (rights fs)

simpleInfo :: Environment -> Clazz Type -> Result ()
simpleInfo env (Clazz t u _) = do
	__$ cmt ("Static type information for class '" ++ tName t ++ "'.")
	__$ dir ".data"
	__$ lbl (mangleClazz t "clazz")
	__$ dir (".word " ++ mangleClazz t "name")
	__$ dir (".word " ++ show (envS env t))
	if (t == typeObject) then
		__$ dir (".word 0")
	else
		__$ dir (".word " ++ mangleClazz (maybe typeObject id u) "clazz")
	__$ dir (".word " ++ mangleMethod t nameInit)
	mapM_ vtblEntry (envMs env t)
	__$ lbl (mangleClazz t "name")
	__$ dir (".asciiz \"" ++ tName t ++ "\"")
	__$ dir ".text"
	where
		vtblEntry (s,_,t) = __$ dir (".word " ++ mangleMethod t s)

simpleInit :: Environment -> Clazz Type -> Result ()
simpleInit env (Clazz t u fs) = do
	__$ cmt ("Implementation of instance initializer for class '" ++ tName t ++ "'.")
	__$ lbl (mangleMethod t nameInit)
	let es = map makeAssign $ filter filterEmpty (lefts fs)
	if null es then
		if isJust u then do
			let superInit = mangleMethod (fromJust u) nameInit
			__$ j superInit						-- jump to super initializer
		else
			__$ jr ra							-- return to caller
	else
		simpleBody env nameInit [] u (BlockExpr typeObject es)
	where
		filterEmpty (Attribute _ _ e) = isJust e
		makeAssign (Attribute s t (Just e)) = AssignmentExpr (annotation e) s e

simpleMethod :: Environment -> Method Type -> Result ()
simpleMethod env (Method s ps _ e) = do
	__$ cmt ("Implementation of method '" ++ s ++ "' in class '" ++ tName (envC env) ++ "'.")
	__$ lbl (mangleMethod (envC env) s)
	simpleBody env s ps Nothing e

simpleBody :: Environment -> String -> [(String,Type)] -> Maybe Type -> Expression Type -> Result ()
simpleBody env s ps u e = do
	let envV = (nameSelf,(SelfLocation,typeSELF)) : zipWith (\(a,b) c -> (a,(c,b))) ps (map ArgumentLocation [1..])
	let env' = envPushV envV env
	__$ sw fp sp (-frameFPOffset)				-- store away old FP
	__$ sw ra sp (-frameRAOffset)				-- store away old RA
	__$ move fp sp								-- keep FP to current frame
	__$ addiu sp sp (-frameSize)				-- reserve stack frame
	when (isJust u && isNothing (lookupClass (inheritableClasses typeObject) (fromJust u))) $
		__$ jal (mangleMethod (fromJust u) s)	-- jump to super method
	state <- simpleExpr (stateInit s) env' e	-- evaluate expression
	__$ addiu sp sp frameSize					-- pop stack frame
	__$ lw ra sp (-frameRAOffset)				-- restore original RA
	__$ lw fp sp (-frameFPOffset)				-- restore original FP
	__$ jr ra									-- return to caller
	simpleConstantPool state env

simpleConstantPool :: State -> Environment -> Result ()
simpleConstantPool state env = do
	if null (stateC state) then
		return ()
	else do
		let s = stateM state
		__$ cmt ("Constant pool of method '" ++ s ++ "' in class '" ++ tName (envC env) ++ "'.")
		__$ dir ".data"
		mapM dumpConstant (stateC state)
		__$ dir ".text"
	where dumpConstant (l,s) = do
		__$ lbl l
		if (elem '\\' s) then					-- ugly hack around SPIM bug!
			__$ dir (".byte " ++ (concat $ intersperse ", " $ map show (map ord s ++ [0])))
		else
			__$ dir (".asciiz " ++ show s)

simpleExpr :: State -> Environment -> Expression Type -> Result State
simpleExpr state env (ConstantExpr _ (Boolean b)) = do
	__$ li v0 (if b then 1 else 0)				-- load constant boolean
	return state
simpleExpr state env (ConstantExpr _ (Integer _ i)) = do
	__$ li v0 i									-- load constant integer
	return state
simpleExpr state env (ConstantExpr _ (String "")) = do
	__$ li v0 0									-- load constant empty string
	return state
simpleExpr state env (ConstantExpr _ (String s)) = do
	let label = uniqueLabel (envC env) (stateM state) (stateL state) "str"
	let state' = statePushL 1 $ statePushC (label,s) state
	__$ la v0 label								-- load constant string
	return state'
simpleExpr state env (IdentifierExpr _ s) = do
	case fst (envV env s) of
		ObjectLocation i -> do
			let offset = (i * sizeOfPointer) + objectFieldOffset
			__$ lw v0 v1 offset					-- load object slot
		ArgumentLocation i -> do
			let offset = (i * sizeOfPointer)
			__$ lw v0 fp offset					-- load argument slot
		StackLocation i -> do
			let offset = -(i * sizeOfPointer) - frameSize
			__$ lw v0 fp offset					-- load stack slot
		SelfLocation -> do
			__$ move v0 v1						-- load self variable
	return state
simpleExpr state env (AssignmentExpr _ s e) = do
	state' <- simpleExpr state env e
	autoBox (annotation e) (snd (envV env s))
	case fst (envV env s) of
		ObjectLocation i -> do
			let offset = (i * sizeOfPointer) + objectFieldOffset
			__$ sw v0 v1 offset					-- store object slot
		ArgumentLocation i -> do
			let offset = (i * sizeOfPointer)
			__$ sw v0 fp offset					-- store argument slot
		StackLocation i -> do
			let offset = -(i * sizeOfPointer) - frameSize
			__$ sw v0 fp offset					-- store stack slot
	return state'
simpleExpr state env (DispatchExpr _ e t s es) = do
	let t' = maybe (fixType env $ annotation e) id t
	let m' = envM env t' s
	let methodOffset = fst m' * sizeOfPointer + clazzVTblOffset
	let argumentSize = toInteger (length es + 1) * sizeOfPointer
	__$ sw v1 sp 0								-- save original 'self'
	__$ addiu sp sp (-argumentSize)				-- reserve arguments frame
	state1 <- stepArgs 1 state env es			-- evaluate arguments
	state2 <- simpleExpr state1 env e			-- evaluate receiver
	autoBox (annotation e) typeObject
	__$ beqz v0 "_cool_error_void_dispatch"		-- check for void dispatch
	__$ move v1 v0								-- change 'self' variable
	if isJust t then do
		__$ jal (mangleMethod (snd m') s)		-- jump to static method
	else do
		__$ lw a0 v1 objectClazzOffset			-- load object class
		__$ lw a0 a0 methodOffset				-- load method pointer
		__$ jalr a0								-- jump to loaded method
	when (s == nameCopy) (autoUnbox typeSELF t')
	__$ addiu sp sp argumentSize				-- pop arguments frame
	__$ lw v1 sp 0								-- restore original 'self'
	return state2
	where
		fixType env t = if (t == typeSELF) then envC env else t
		stepArgs arg state env [] = return state
		stepArgs arg state env (e:es) = do
			state1 <- simpleExpr state env e	-- evaluate argument
			__$ sw v0 sp (arg * sizeOfPointer)	-- pass argument via stack
			state2 <- stepArgs (arg+1) state1 env es
			return state2
simpleExpr state env (ConditionalExpr _ e1 e2 e3) = do
	let label1 = (uniqueLabel (envC env) (stateM state) (stateL state) "false")
	let label2 = (uniqueLabel (envC env) (stateM state) (stateL state) "endif")
	let state' = statePushL 1 state
	state1 <- simpleExpr state' env e1			-- evaluate condition
	__$ beqz v0 label1							-- branch to false label
	state2 <- simpleExpr state1 env e2			-- evaluate true branch
	__$ j label2								-- jump to end label
	__$ lbl label1
	state3 <- simpleExpr state2 env e3			-- evaluate false branch
	__$ lbl label2
	return state3
simpleExpr state env (LoopExpr _ e1 e2) = do
	let label1 = (uniqueLabel (envC env) (stateM state) (stateL state) "head")
	let label2 = (uniqueLabel (envC env) (stateM state) (stateL state) "tail")
	let state' = statePushL 1 state
	__$ lbl label1
	state1 <- simpleExpr state' env e1			-- evaluate condition
	__$ beqz v0 label2							-- branch to tail label
	state2 <- simpleExpr state1 env e2			-- evaluate loop body
	__$ j label1								-- jump to head label
	__$ lbl label2
	__$ move v0 zero							-- always return void
	return state2
simpleExpr state env (BlockExpr _ es) = do
	state' <- stepBlock state env es			-- evaluate expressions
	return state'
	where
		stepBlock state env [] = return state
		stepBlock state env (e:es) = do
			state1 <- simpleExpr state env e	-- evaluate expression
			state2 <- stepBlock state1 env es
			return state2
simpleExpr state env (LetExpr _ s t e1 e2) = do
	state1 <- maybe emptyInit (simpleExpr state env) e1
	autoBox (maybe t annotation e1) t
	__$ sw v0 sp 0								-- save value in stack slot
	__$ addiu sp sp (-sizeOfPointer)			-- reserve stack slot
	let depth = envD env
	let env' = envPushD $ envPushV [(s,(StackLocation depth,t))] env
	state2 <- simpleExpr state1 env' e2			-- evaluate 2nd expression
	__$ addiu sp sp sizeOfPointer				-- pop stack slot
	return state2
	where emptyInit = do
		__$ move v0 zero						-- initialize to void
		return state
simpleExpr state env (CaseExpr _ e cs) = do
	let label1 = (uniqueLabel (envC env) (stateM state) (stateL state) "case")
	let label2 = (uniqueLabel (envC env) (stateM state) (stateL state) "esac")
	let label3 t = (uniqueLabel (envC env) (stateM state) (stateL state) ("t" ++ tName t))
	let cases = map (\(s,t,e) -> (s,t,e,label3 t)) cs
	let state' = statePushL 1 state
	state1 <- simpleExpr state' env e			-- evaluate expression
	__$ sw v0 sp 0								-- save value in stack slot
	__$ addiu sp sp (-sizeOfPointer)			-- reserve stack slot
	autoBox (annotation e) typeObject
	__$ beqz v0 "_cool_error_void_case"			-- check for void case
	__$ lw a0 v0 objectClazzOffset				-- load object class
	__$ lbl label1
	state2 <- stepCases label2 state1 env cases	-- evaluate all cases
	__$ lw a0 a0 clazzPrntOffset				-- load super class
	__$ bne a0 zero label1						-- maybe retry super class
	__$ j "_cool_error_unmatched_case"			-- hit an unmatched case
	__$ lbl label2
	__$ addiu sp sp sizeOfPointer				-- pop stack slot
	return state2
	where
		stepCases esac state env [] = return state
		stepCases esac state env ((s,t,e,l):cs) = do
			let depth = envD env
			let env' = envPushD $ envPushV [(s,(StackLocation depth,t))] env
			__$ la a1 (mangleClazz t "clazz")	-- load class info address
			__$ bne a0 a1 l						-- check for matching case
			state1 <- simpleExpr state env' e	-- evaluate expression
			__$ j esac							-- jump to end of all cases
			__$ lbl l
			state2 <- stepCases esac state1 env cs
			return state2
simpleExpr state env (NewExpr _ t)
	| (t `elem` [typeInt,typeBool,typeString]) = do
		__$ move v0 zero						-- always return void
		return state
	| (t == typeSELF) = do
		__$ sw v1 sp 0							-- save original 'self'
		__$ addiu sp sp (-sizeOfPointer)		-- reserve stack slot
		__$ lw a0 v1 objectClazzOffset			-- load object class
		__$ lw a1 a0 clazzSizeOffset			-- load instance size
		allocateDynamic a1 v1					-- allocate new instance
		__$ sw a0 v1 objectClazzOffset			-- set object class
		__$ lw a0 a0 clazzInitOffset			-- load initializer method
		__$ jalr a0								-- jump to initializer
		__$ move v0 v1							-- return new instance
		__$ addiu sp sp sizeOfPointer			-- pop stack slot
		__$ lw v1 sp 0							-- restore original 'self'
		return state
	| otherwise = do
		__$ sw v1 sp 0							-- save original 'self'
		__$ addiu sp sp (-sizeOfPointer)		-- reserve stack slot
		allocateStatic (envS env t) v1			-- allocate new instance
		__$ la a0 (mangleClazz t "clazz")		-- load class info address
		__$ sw a0 v1 objectClazzOffset			-- set object class
		__$ jal (mangleMethod t nameInit)		-- jump to instance initializer
		__$ move v0 v1							-- return new instance
		__$ addiu sp sp sizeOfPointer			-- pop stack slot
		__$ lw v1 sp 0							-- restore original 'self'
		return state
simpleExpr state env (IsVoidExpr _ e) = do
	state' <- simpleExpr state env e			-- evaluate expression
	__$ seq_ v0 v0 zero							-- check for void pointer
	return state'
simpleExpr state env (UnaryOpExpr _ op e) = do
	state' <- simpleExpr state env e			-- evaluate expression
	case op of
		IntNegOp  -> __$ neg v0 v0				-- arithmetic negation
		BoolNegOp -> __$ seq_ v0 v0 zero		-- logical negation
	return state'
simpleExpr state env (BinaryOpExpr _ op e1 e2) = do
	state1 <- simpleExpr state env e1			-- evaluate 1st expression
	__$ sw v0 sp 0								-- save value of 1st expression
	__$ addiu sp sp (-sizeOfPointer)			-- reserve stack slot
	state2 <- simpleExpr state1 env e2			-- evaluate 2nd expression
	__$ addiu sp sp sizeOfPointer				-- pop stack slot
	__$ lw a0 sp 0								-- restore value of 1st expression
	case op of
		AddOp -> __$ add v0 a0 v0				-- arithmetic addition
		SubOp -> __$ sub v0 a0 v0				-- arithmetic subtraction
		MulOp -> __$ mul v0 a0 v0				-- arithmetic multiplication
		DivOp -> __$ div_ v0 a0 v0				-- arithmetic division
	return state2
simpleExpr state env (ComparisonOpExpr _ op e1 e2) = do
	state1 <- simpleExpr state env e1			-- evaluate 1st expression
	__$ sw v0 sp 0								-- save value of 1st expression
	__$ addiu sp sp (-sizeOfPointer)			-- reserve stack slot
	state2 <- simpleExpr state1 env e2			-- evaluate 2nd expression
	__$ addiu sp sp sizeOfPointer				-- pop stack slot
	__$ lw a0 sp 0								-- restore value of 1st expression
	case op of
		LessOp        -> __$ slt v0 a0 v0		-- arithmetic less than
		LessOrEqualOp -> __$ sle v0 a0 v0		-- arithmetic less or equal
		EqualOp       -> equal (annotation e1)	-- overloaded equality
	return state2
	where equal t
		| (t == typeString) = __$ jal "_streq"	-- string equality
		| otherwise         = __$ seq_ v0 a0 v0	-- generic equality
