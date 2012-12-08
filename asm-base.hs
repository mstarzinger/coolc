-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module Assembler (
		Assembly,
		Directive(..),
		Register(Register),
		cmt, dir, lbl, ins,
	) where

-- The type representing assembler output is a list of directives.
type Assembly a = [Directive a]

-- The data type representing a single assembler directive.
data Directive a =
	Comment String		|
	Directive String	|
	Label String		|
	Instruction a

-- The data type representing a single machine register.
data Register = Register Int String

-- Helper to generate a comment directive.
cmt :: String -> Directive a
cmt s = Comment s

-- Helper to generate a dotted directive.
dir :: String -> Directive a
dir s = Directive s

-- Helper to generate a label directive.
lbl :: String -> Directive a
lbl s = Label s

-- Helper to generate an instruction directive.
ins :: a -> Directive a
ins i = Instruction i
