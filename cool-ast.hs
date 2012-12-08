-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolAST(
		Clazz(..),
		Type(..),
		Attribute(..),
		Method(..),
		Expression(..),
		UnaryOp(..),
		BinaryOp(..),
		ComparisonOp(..),
		annotation,
		no,
		cName,
		cType,
		tName,
	) where
import CoolLexer (Token)

-- The data type representing parsed classes.
data Clazz a = Clazz Type (Maybe Type) [Either (Attribute a) (Method a)]
	deriving Show

-- The data type representing parsed types (class names).
data Type = Type String
	deriving (Eq,Show,Ord)

-- The data type representing parsed attributes (class features).
data Attribute a = Attribute String Type (Maybe (Expression a))
	deriving Show

-- The data type representing parsed methods (class features).
data Method a = Method String [(String,Type)] Type (Expression a)
	deriving Show

-- The data type representing parsed expressions.
data Expression a =
	ConstantExpr a Token												|
	IdentifierExpr a String												|
	AssignmentExpr a String (Expression a)								|
	DispatchExpr a (Expression a) (Maybe Type) String [Expression a]	|
	ConditionalExpr a (Expression a) (Expression a) (Expression a)		|
	LoopExpr a (Expression a) (Expression a)							|
	BlockExpr a [Expression a]											|
	LetExpr a String Type (Maybe (Expression a)) (Expression a)			|
	CaseExpr a (Expression a) [(String,Type,Expression a)]				|
	NewExpr a Type														|
	IsVoidExpr a (Expression a)											|
	UnaryOpExpr a UnaryOp (Expression a)								|
	BinaryOpExpr a BinaryOp (Expression a) (Expression a)				|
	ComparisonOpExpr a ComparisonOp (Expression a) (Expression a)
	deriving Show

-- Some helper data types for operations.
data UnaryOp = IntNegOp | BoolNegOp deriving Show
data BinaryOp = AddOp | SubOp | MulOp | DivOp deriving Show
data ComparisonOp = LessOp | LessOrEqualOp | EqualOp deriving Show

-- Returns the annotation from an expression.
annotation :: Expression a -> a
annotation (ConstantExpr a _) = a
annotation (IdentifierExpr a _) = a
annotation (AssignmentExpr a _ _) = a
annotation (DispatchExpr a _ _ _ _) = a
annotation (ConditionalExpr a _ _ _) = a
annotation (LoopExpr a _ _) = a
annotation (BlockExpr a _) = a
annotation (LetExpr a _ _ _ _) = a
annotation (CaseExpr a _ _) = a
annotation (NewExpr a _) = a
annotation (IsVoidExpr a _) = a
annotation (UnaryOpExpr a _ _) = a
annotation (BinaryOpExpr a _ _ _) = a
annotation (ComparisonOpExpr a _ _ _) = a

-- Empty annotation for the above data types.
no :: ()
no = ()

-- Helper to retrieve name from a class.
cName :: Clazz a -> String
cName = tName . cType

-- Helper to retrieve type from a class.
cType :: Clazz a -> Type
cType (Clazz t _ _) = t

-- Helper to retrieve name from a type.
tName :: Type -> String
tName (Type s) = s
