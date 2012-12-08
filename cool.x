{
-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolLexer (
		Alex(..),
		AlexPosn(..),
		AlexReturn(..),
		AlexState(..),
		Token(..),
		alexLine,
		alexScan,
		alexStartPos,
		scan,
	) where
import Data.Char (intToDigit)
import Numeric (showIntAtBase)
}

%wrapper "monad"

$digit = 0-9							-- Digits
$alpha = [a-zA-Z]						-- Alphabetic characters
$lower = [a-z]							-- Lower case letters
$upper = [A-Z]							-- Upper case letters
$paren = [\{\}\(\)]						-- Symbols for parenthesis
$separ = [\:\;\,\.\@]					-- Symbols for separators
$opers = [\+\-\*\/\~\<\=]				-- Symbols for operators

@chars = [^\"\0\\] | \\[^\0] | \\\n		-- String characters

tokens :-
	$white+								;
	"--".*								;
	"(*"								{ skipNestedComments }
	"*)"								{ errorCommentMissingOpen }

	[cC][aA][sS][eE]					{ mkToken Case }
	[cC][lL][aA][sS][sS]				{ mkToken Class }
	[eE][lL][sS][eE]					{ mkToken Else }
	[eE][sS][aA][cC]					{ mkToken Esac }
	[fF][iI]							{ mkToken Fi }
	[iI][fF]							{ mkToken If }
	[iI][nN]							{ mkToken In }
	[iI][nN][hH][eE][rR][iI][tT][sS]	{ mkToken Inherits }
	[iI][sS][vV][oO][iI][dD]			{ mkToken IsVoid }
	[lL][eE][tT]						{ mkToken Let }
	[lL][oO][oO][pP]					{ mkToken Loop }
	[nN][eE][wW]						{ mkToken New }
	[nN][oO][tT]						{ mkToken Not }
	[oO][fF]							{ mkToken Of }
	[pP][oO][oO][lL]					{ mkToken Pool }
	[tT][hH][eE][nN]					{ mkToken Then }
	[wW][hH][iI][lL][eE]				{ mkToken While }

	f[aA][lL][sS][eE]					{ mkToken (Boolean False) }
	t[rR][uU][eE]						{ mkToken (Boolean True) }

	$digit+								{ mkInteger }
	$upper [$alpha $digit \_]*			{ mkTypeIdentifier }
	$lower [$alpha $digit \_]*			{ mkObjectIdentifier }

	\" @chars* \"						{ mkString }
	\" @chars* 							{ errorStringEof }
	\" @chars* \\						{ errorStringEscapedEof }
	\" @chars* \n						{ errorStringNewline }
	\" (@chars | \0)* [\"\n]			{ errorStringNull }
	\" (@chars | \\\0)* [\"\n]			{ errorStringEscapedNull }

	$paren | $separ | $opers			{ mkSymbol }
	"=>"								{ mkToken Arrow }
	"<-"								{ mkToken Assign }
	"<="								{ mkToken LessOrEqual }

	.									{ \a@(_,_,s) -> mkError (take 1 s) a }

{
-- The token type enumerates all possible kinds of lexical tokens that
-- this scanner can produce. Some tokens contain additional information
-- about the underlying lexeme that they were derived from.
data Token =
	TypeIdentifier String		|	-- Type identifiers
	ObjectIdentifier String		|	-- Object identifiers
	Boolean Bool				|	-- Boolean constants
	Integer String Integer		|	-- Integer constants
	String String				|	-- String constants
	Symbol Char					|	-- Special syntactic symbols
	Arrow						|
	Assign						|
	LessOrEqual					|
	Case						|	-- Keywords
	Class						|
	Else						|
	Esac						|
	Fi							|
	If							|
	In							|
	Inherits					|
	IsVoid						|
	Let							|
	Loop						|
	New							|
	Not							|
	Of							|
	Pool						|
	Then						|
	While						|
	Error String				|	-- Artificial error token
	EndOfFile						-- Artificial end-of-file token.
		deriving Eq

-- The tokens can be converted into strings to allow easy printing for
-- debugging purposes and to run the scanner as a standalone program
-- that prints tokens to the console.
instance Show Token where
	show (TypeIdentifier id)	= "TYPEID " ++ id
	show (ObjectIdentifier id)	= "OBJECTID " ++ id
	show (Boolean bool)			= "BOOL_CONST " ++ (show bool)
	show (Integer lex _)		= "INT_CONST " ++ lex
	show (String str)			= "STR_CONST \"" ++ (escape str) ++ "\""
	show (Symbol char)			= show char
	show Arrow					= "DARROW"
	show Assign					= "ASSIGN"
	show LessOrEqual			= "LE"
	show Case					= "CASE"
	show Class					= "CLASS"
	show Else					= "ELSE"
	show Esac					= "ESAC"
	show Fi						= "FI"
	show If						= "IF"
	show In						= "IN"
	show Inherits				= "INHERITS"
	show IsVoid					= "ISVOID"
	show Let					= "LET"
	show Loop					= "LOOP"
	show New					= "NEW"
	show Not					= "NOT"
	show Of						= "OF"
	show Pool					= "POOL"
	show Then					= "THEN"
	show While					= "WHILE"
	show (Error msg)			= "ERROR " ++ (show msg)
	show EndOfFile				= "EOF"

-- The positioned token associates line number information with a given
-- token class to allow mapping to a specific position in the input.
data PositionedToken = PositionedToken AlexPosn Token
instance Show PositionedToken where
	show (PositionedToken (AlexPn _ l _) t) = "#" ++ (show l) ++ " " ++ (show t)

-- Utility function to create arbitrary tokens.
mkToken :: Token -> AlexInput -> Int -> Alex Token
mkToken t _ _ = return t

-- Utility functions to create identifier tokens.
mkTypeIdentifier :: AlexInput -> Int -> Alex Token
mkTypeIdentifier (_,_,s) l = return (TypeIdentifier $ take l s)
mkObjectIdentifier :: AlexInput -> Int -> Alex Token
mkObjectIdentifier (_,_,s) l = return (ObjectIdentifier $ take l s)

-- Utility function to create integer tokens.
mkInteger :: AlexInput -> Int -> Alex Token
mkInteger (_,_,s) l = return (Integer (take l s) (read $ take l s))

-- Utility function to create string tokens.
mkString :: AlexInput -> Int -> Alex Token
mkString a@(_,_,s) l = let decoded = (unescape $ init $ tail $ take l s) in
	if (length decoded) > 1024
		then errorStringTooLong a l
		else return (String decoded)

-- Utility function to create symbol tokens.
mkSymbol :: AlexInput -> Int -> Alex Token
mkSymbol (_,_,s) _ = return (Symbol $ head s)

-- Utility function to create error tokens.
mkError :: String -> AlexInput -> Int -> Alex Token
mkError s _ _ = return (Error s)

-- Utility function to skip (nested) multiline comments.
skipNestedComments :: AlexInput -> Int -> Alex Token
skipNestedComments a l = do
	input <- alexGetInput
	go 1 input where
		go 0 input = do alexSetInput input; alexMonadScan
		go n input = do
			let prevChar = alexInputPrevChar input
			case alexGetChar input of
				Nothing          -> err input
				Just (')',input) -> do
					case prevChar of
						'*' -> go (n-1) input
						c   -> go n input
				Just ('*',input) -> do
					case prevChar of
						'(' -> go (n+1) input
						c   -> go n input
				Just (c,input)   -> go n input
		err input = do alexSetInput input; errorCommentMissingClose a l

-- These are predefined error tokens that represent various lexical
-- errors that might occur while scanning input.
errorStringTooLong			= mkError "String constant too long, exceeds 1024 character limit."
errorStringNewline			= mkError "String constant not terminated, contains newline character."
errorStringEof				= mkError "String constant not terminated, contains EOF character."
errorStringEscapedEof		= mkError "String constant not terminated, contains escaped EOF character."
errorStringNull				= mkError "String constant contains null character."
errorStringEscapedNull		= mkError "String constant contains escaped null character."
errorCommentMissingClose	= mkError "Comment misses close sequence, found EOF in comment."
errorCommentMissingOpen		= mkError "Comment misses open sequence, unmatched *) found."

-- These are helper functions that either escape or unescape a string
-- constant so that it can be printed or scanned properly.
escape :: String -> String
escape ('\b':xs)	= "\\b" ++ escape xs
escape ('\f':xs)	= "\\f" ++ escape xs
escape ('\n':xs)	= "\\n" ++ escape xs
escape ('\t':xs)	= "\\t" ++ escape xs
escape ('\\':xs)	= "\\\\" ++ escape xs
escape ('"':xs)		= "\\\"" ++ escape xs
escape (x:xs)		= escapeChar x ++ escape xs
escape ""			= ""
escapeChar c = if (ord c) < 32 then showOctal (ord c) else [c]
showOctal i = "\\0" ++ showIntAtBase 8 intToDigit i ""
unescape :: String -> String
unescape ('\\':'b':xs)	= '\b' : unescape xs
unescape ('\\':'f':xs)	= '\f' : unescape xs
unescape ('\\':'n':xs)	= '\n' : unescape xs
unescape ('\\':'t':xs)	= '\t' : unescape xs
unescape ('\\':x:xs)	= x : unescape xs
unescape (x:xs)			= x : unescape xs
unescape ""				= ""

-- This end-of-file definition is needed by Alex when using the monad
-- wrapper in order to mark the end of the scanned input.
alexEOF :: Alex Token
alexEOF = return EndOfFile

-- This is a helper function that gets the line number of a position.
alexLine :: AlexPosn -> Int
alexLine (AlexPn _ l _) = l

-- This is a helper function that returns the current scanner position.
alexPosition :: Alex AlexPosn
alexPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

-- This is the main loop going through the input and producing either
-- an error message or a list of tokens that represent the input.
scan :: String -> Either String [PositionedToken]
scan str = runAlex str $ do
	let loop = do
		token <- alexMonadScan
		position <- alexPosition
		let t = PositionedToken position token
		if token == EndOfFile
			then return []
			else do tokens <- loop; return (t : tokens)
	loop
}
