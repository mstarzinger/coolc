-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module Main (main) where
import CoolSemant (Result(..),semant)
import CoolPrinter (prettyProgram)
import System.Console.GetOpt
import System.Environment (getArgs)

-- This is a description of the command line options to this program.
options :: [OptDescr ()]
options = [
		Option ['g'] ["garbage"] (NoArg ()) "gabage collection",
		Option ['o'] ["output"] (ReqArg (const ()) "FILE") "output file"
	]

-- This is the main program entry for the semantic analyzer used as a
-- standalone program that reads input from a file and prints resulting
-- type-annotated AST.
main = do
	args <- getArgs
	filename <- do
		case getOpt RequireOrder options args of
			(_,[x],[]) -> return x
			(_,___,__) -> error $ usageInfo "Invalid usage." options
	input <- readFile filename
	case semant input of
		(Program p)         -> prettyProgram p
		(LexerError l m)    -> printLexerError filename l m
		(ParserError l m)   -> printParseError filename l m
		(SemanticError l m) -> printSemanError filename l m
		(GenericError m)    -> printGenerError filename m
	where
		printLexerError f l m = do
			putStr $ f ++ ":" ++ (show l) ++ ": lexical error " ++ (show m) ++ "\n"
			putStr $ "Compilation halted due to lex and parse errors\n"
		printParseError f l m = do
			putStr $ f ++ ":" ++ (show l) ++ ": syntax error at or near " ++ (show m) ++ "\n"
			putStr $ "Compilation halted due to lex and parse errors\n"
		printSemanError f l m = do
			putStr $ f ++ ":" ++ (show l) ++ ": " ++ m ++ "\n"
			putStr $ "Compilation halted due to static semantic errors.\n"
		printGenerError f m = do
			putStr $ m ++ "\n"
			putStr $ "Compilation halted due to static semantic errors.\n"
