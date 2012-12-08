-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module Assembler.Printer (
		prettyAsm,
	) where
import Assembler
import System.IO (Handle,hPutStr)

prettyAsm :: (Show a) => Assembly a -> Handle -> IO ()
prettyAsm ds h = do
	hPutStr h "# This file was automatically generated by the Cool compiler.\n"
	hPutStr h "# (c) Copyright 2012 Michael Starzinger. All Rights Reserved.\n"
	mapM_ (hPutStr h . prettyDirective) ds

prettyDirective :: (Show a) => Directive a -> String
prettyDirective (Comment s)     = "\n# " ++ s ++ "\n"
prettyDirective (Directive s)   = "\t" ++ s ++ "\n"
prettyDirective (Label s)       = " " ++ s ++ ":\n"
prettyDirective (Instruction i) = "\t" ++ (show i) ++ "\n"
