-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module Main (main) where
import CoolLexer (scan)
import System.Console.GetOpt
import System.Environment (getArgs)

-- This is a description of the command line options to this program.
options :: [OptDescr ()]
options = [
		Option ['g'] ["garbage"] (NoArg ()) "gabage collection",
		Option ['o'] ["output"] (ReqArg (const ()) "FILE") "output file"
	]

-- This is the main program entry for the scanner used as a standalone
-- program that reads input from a file and prints resulting tokens.
main = do
	args <- getArgs
	filename <- do
		case getOpt RequireOrder options args of
			(_,[x],[]) -> return x
			(_,___,__) -> error $ usageInfo "Invalid usage." options
	input <- readFile filename
	mapM print $ either error id $ scan input
