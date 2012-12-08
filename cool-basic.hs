-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolBasic (
		definedClasses,
		inheritableClasses,
		nameSelf,
		nameInit,
		nameMain,
		nameAbort,
		nameTypeName,
		nameCopy,
		nameOutputString,
		nameOutputInt,
		nameInputString,
		nameInputInt,
		nameLength,
		nameConcat,
		nameSubstr,
		typeSELF,
		typeObject,
		typeMain,
		typeIO,
		typeInt,
		typeBool,
		typeString,
	) where
import CoolAST
import CoolLexer

-- List of basic classes that are defined by the runtime.
definedClasses :: a -> [Clazz a]
definedClasses a = [ base a, baseIO a, baseInt a, baseBool a, baseString a ]

-- List of basic inheritable classes present in the runtime.
inheritableClasses :: a -> [Clazz a]
inheritableClasses a = [ base a, baseIO a ]

-- Basic variable names.
nameSelf = "self"

-- Basic method names.
nameInit = ".init"
nameMain = "main"
nameAbort = "abort"
nameTypeName = "type_name"
nameCopy = "copy"
nameOutputString = "out_string"
nameOutputInt = "out_int"
nameInputString = "in_string"
nameInputInt = "in_int"
nameLength = "length"
nameConcat = "concat"
nameSubstr = "substr"

-- Basic type constants.
typeSELF = Type "SELF_TYPE"
typeObject = Type "Object"
typeMain = Type "Main"
typeIO = Type "IO"
typeInt = Type "Int"
typeBool = Type "Bool"
typeString = Type "String"

base a = Clazz typeObject Nothing [ m1,m2,m3 ]
	where
		m1 = Right $ Method nameAbort [] typeObject (IdentifierExpr a nameSelf)
		m2 = Right $ Method nameTypeName [] typeString (ConstantExpr a (String ""))
		m3 = Right $ Method nameCopy [] typeSELF (IdentifierExpr a nameSelf)

baseIO a = Clazz typeIO Nothing [ m1,m2,m3,m4 ]
	where
		m1 = Right $ Method nameOutputString [("x",typeString)] typeSELF (IdentifierExpr a nameSelf)
		m2 = Right $ Method nameOutputInt [("x",typeInt)] typeSELF (IdentifierExpr a nameSelf)
		m3 = Right $ Method nameInputString [] typeString (ConstantExpr a (String ""))
		m4 = Right $ Method nameInputInt [] typeInt (ConstantExpr a (Integer "0" 0))

baseInt _ = Clazz typeInt Nothing [ f1 ]
	where
		f1 = Left $ Attribute "value" typeInt Nothing

baseBool _ = Clazz typeBool Nothing [ f1 ]
	where
		f1 = Left $ Attribute "value" typeBool Nothing

baseString a = Clazz typeString Nothing [ f1,m1,m2,m3 ]
	where
		f1 = Left $ Attribute "value" typeString Nothing
		m1 = Right $ Method nameLength [] typeInt (ConstantExpr a (Integer "0" 0))
		m2 = Right $ Method nameConcat [("s",typeString)] typeString (IdentifierExpr a nameSelf)
		m3 = Right $ Method nameSubstr [("i",typeInt),("l",typeInt)] typeString (IdentifierExpr a nameSelf)
