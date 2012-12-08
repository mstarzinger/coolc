-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module Main (main) where
import Assembler.Printer (prettyAsm)
import CoolCodegen (generate)
import CoolSemant (Result(..),semant)
import Data.List (find)
import Data.Maybe (fromJust,isJust)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO (IOMode(WriteMode),withFile)

-- This is a description of the command line options to this program.
options :: [OptDescr (Maybe String)]
options = [
		Option ['g'] ["garbage"] (NoArg Nothing) "gabage collection",
		Option ['o'] ["output"] (ReqArg Just "FILE") "output file"
	]

-- This is the main program entry for the code generator used as a
-- standalone program that reads input from a file and ...
main = do
	args <- getArgs
	(filename1, filename2) <- do
		case getOpt RequireOrder options args of
			(o,[x],[]) -> do
				let def = x ++ ".s"
				let out = maybe def fromJust (find isJust o)
				return (x, out)
			(_,___,__) -> error $ usageInfo "Invalid usage." options
	input <- readFile filename1
	case semant input of
		(Program p)         -> withFile filename2 WriteMode $ prettyAsm $ generate p
		(LexerError _ m)    -> error $ "Lexical Error: " ++ m
		(ParserError _ m)   -> error $ "Syntax Error: " ++ m
		(SemanticError _ m) -> error $ "Semantical Error: " ++ m
		(GenericError m)    -> error $ "Generic Error: " ++ m
