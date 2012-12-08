{
-- Solution to Stanford Compilers Course.
-- (c) Copyright 2012 Michael Starzinger. All Rights Reserved.

module CoolParser (
		Result(..),
		parse
	) where
import CoolAST
import CoolLexer
}

%name parseCool
%monad { State } { parseThen } { parseReturn }
%error { parseError }
%lexer { lexer } { EndOfFile }
%tokentype { Token }

%token
	type		{ TypeIdentifier $$ }
	id			{ ObjectIdentifier $$ }
	false		{ Boolean False }
	true		{ Boolean True }
	integer		{ Integer _ _ }
	string		{ String _ }
	'{'			{ Symbol '{' }
	'}'			{ Symbol '}' }
	'('			{ Symbol '(' }
	')'			{ Symbol ')' }
	':'			{ Symbol ':' }
	';'			{ Symbol ';' }
	','			{ Symbol ',' }
	'.'			{ Symbol '.' }
	'@'			{ Symbol '@' }
	'+'			{ Symbol '+' }
	'-'			{ Symbol '-' }
	'*'			{ Symbol '*' }
	'/'			{ Symbol '/' }
	'~'			{ Symbol '~' }
	'<'			{ Symbol '<' }
	'='			{ Symbol '=' }
	'=>'		{ Arrow }
	'<-'		{ Assign }
	'<='		{ LessOrEqual }
	case		{ Case }
	class		{ Class }
	else		{ Else }
	esac		{ Esac }
	fi			{ Fi }
	if			{ If }
	in			{ In }
	inherits	{ Inherits }
	isvoid		{ IsVoid }
	let			{ Let }
	loop		{ Loop }
	new			{ New }
	not			{ Not }
	of			{ Of }
	pool		{ Pool }
	then		{ Then }
	while		{ While }

%right		'<-'
%nonassoc	not
%nonassoc	'<=' '<' '='
%left		'+' '-'
%left		'*' '/'
%nonassoc	isvoid
%nonassoc	'~'
%nonassoc	'@'
%nonassoc	'.'

%% -- Like yacc, we include %% here, for no real reason.

Program
	: ClassSeq										{ $1 }

ClassSeq
	: Class ';'										{ [$1] }
	| Class ';' ClassSeq							{ $1 : $3 }

Class
	: class type InheritsOpt '{' FeatureSeq '}'		{ Clazz (Type $2) $3 $5 }

InheritsOpt
	: {- empty -}									{ Nothing }
	| inherits type									{ Just (Type $2) }

FeatureSeq
	: {- empty -}									{ [] }
	| Feature ';' FeatureSeq						{ $1 : $3 }

Feature
	: id '(' ParameterSeq ')' ':' type '{' Expr '}'	{ Right $ Method $1 $3 (Type $6) $8 }
	| id ':' type AssignOpt							{ Left $ Attribute $1 (Type $3) $4 }

ParameterSeq
	: {- empty -}									{ [] }
	| Formal ParameterSeq2							{ $1 : $2 }

ParameterSeq2
	: {- empty -}									{ [] }
	| ',' Formal ParameterSeq2						{ $2 : $3 }

AssignOpt
	: {- empty -}									{ Nothing }
	| '<-' Expr										{ Just $2 }

Formal
	: id ':' type									{ ($1, Type $3) }

Expr
	: id '<-' Expr									{ AssignmentExpr no $1 $3 }
	| Expr AtTypeOpt '.' id '(' ArgumentSeq ')'		{ DispatchExpr no $1 $2 $4 $6 }
	| id '(' ArgumentSeq ')'						{ DispatchExpr no (IdentifierExpr no "self") Nothing $1 $3 }
	| if Expr then Expr else Expr fi				{ ConditionalExpr no $2 $4 $6 }
	| while Expr loop Expr pool						{ LoopExpr no $2 $4 }
	| '{' ExprSeq '}'								{ BlockExpr no $2 }
	| let LetSeq in Expr							{ desugarLetExpr $2 $4 }
	| case Expr of CaseSeq esac						{ CaseExpr no $2 $4 }
	| new type										{ NewExpr no (Type $2) }
	| isvoid Expr									{ IsVoidExpr no $2 }
	| Expr '+' Expr									{ BinaryOpExpr no AddOp $1 $3 }
	| Expr '-' Expr									{ BinaryOpExpr no SubOp $1 $3 }
	| Expr '*' Expr									{ BinaryOpExpr no MulOp $1 $3 }
	| Expr '/' Expr									{ BinaryOpExpr no DivOp $1 $3 }
	| '~' Expr										{ UnaryOpExpr no IntNegOp $2 }
	| Expr '<' Expr									{ ComparisonOpExpr no LessOp $1 $3 }
	| Expr '<=' Expr								{ ComparisonOpExpr no LessOrEqualOp $1 $3 }
	| Expr '=' Expr									{ ComparisonOpExpr no EqualOp $1 $3 }
	| not Expr										{ UnaryOpExpr no BoolNegOp $2 }
	| '(' Expr ')'									{ $2 }
	| id											{ IdentifierExpr no $1 }
	| integer										{ ConstantExpr no $1 }
	| string										{ ConstantExpr no $1 }
	| true											{ ConstantExpr no $1 }
	| false											{ ConstantExpr no $1 }

AtTypeOpt
	: {- empty -}									{ Nothing }
	| '@' type										{ Just (Type $2) }

ArgumentSeq
	: {- empty -}									{ [] }
	| Expr ArgumentSeq2								{ $1 : $2 }

ArgumentSeq2
	: {- empty -}									{ [] }
	| ',' Expr ArgumentSeq2							{ $2 : $3 }

ExprSeq
	: Expr ';'										{ [$1] }
	| Expr ';' ExprSeq								{ $1 : $3 }

LetSeq
	: id ':' type AssignOpt LetSeq2					{ ($1,(Type $3),$4) : $5 }

LetSeq2
	: {- empty -}									{ [] }
	| ',' id ':' type AssignOpt LetSeq2				{ ($2,(Type $4),$5) : $6 }

CaseSeq
	: id ':' type '=>' Expr ';'						{ ($1,Type $3,$5) : [] }
	| id ':' type '=>' Expr ';' CaseSeq				{ ($1,Type $3,$5) : $7 }

{
-- The data type representing parsed result. Can be used as a monad if
-- the appropriate bind and return operations are provided.
data Result a =
	LexerError Int String		|
	ParserError Int Token		|
	Program a

-- This helper de-sugars one let expression with multiple bindings to
-- multiple let expressions with one binding each, which is semantically
-- equivalent for the given language.
desugarLetExpr :: [(String,Type,Maybe (Expression ()))] -> Expression () -> Expression ()
desugarLetExpr ((s,t,a):xs) e = LetExpr no s t a (desugarLetExpr xs e)
desugarLetExpr []           e = e

-- This is the parser state of our handcrafted parser monad.
type State a = AlexState -> Result a

-- The monadic bind operation for our handcrafted parser monad.
parseThen :: State a -> (a -> State b) -> State b
parseThen m k = \x -> case m x of
	(LexerError l m)  -> LexerError l m
	(ParserError l t) -> ParserError l t
	(Program p)       -> k p x

-- The monadic return operation for our handcrafted parser monad.
parseReturn :: a -> State a
parseReturn p = \x -> Program p

-- This helper function is needed by Happy and will be called in the
-- event of a parse error. Note that this function must be polymorphic
-- in its return type.
parseError :: Token -> State a
parseError EndOfFile _               = ParserError 0 EndOfFile -- ugly hack!
parseError t AlexState{alex_pos=pos} = ParserError (alexLine pos) t

-- Helper method for the lexer that creates a lexer state.
lexerState :: (AlexPosn,String,Char,Int) -> AlexState
lexerState (pos,inp,chr,scd) = AlexState{alex_pos=pos,alex_inp=inp,alex_chr=chr,alex_scd=scd}

-- Helper method for the lexer that creates an initial lexer state.
lexerStart :: String -> AlexState
lexerStart s = lexerState (alexStartPos,s,'\n',0)

-- This is the main threaded lexer method and it takes a continuation
-- function into which it feeds scanned tokens. Note that this method is
-- based on our handcrafted parser monad, but internally dispatches to
-- the automatically generated lexer monad.
lexer :: (Token -> State a) -> State a
lexer cont s@AlexState{alex_pos=pos,alex_inp=inp,alex_chr=chr,alex_scd=scd} =
	let input = (pos,chr,inp) in
	case alexScan input scd of
		AlexEOF                         -> cont EndOfFile s
		AlexSkip (pos,chr,inp) _        -> lexer cont $ lexerState (pos,inp,chr,scd)
		AlexError (pos,_,_)             -> LexerError (alexLine pos) "Internal lexer error."
		AlexToken (pos,chr,inp) len act ->
			let monad = (act input len) in
			case unAlex monad $ lexerState (pos,inp,chr,scd) of
				Left msg        -> LexerError (alexLine pos) ("Internal lexer error: " ++ msg)
				Right (s', tok) -> case tok of
					Error msg -> LexerError (alexLine pos) msg
					token     -> cont token s'

-- This is the main entry point for the parser.
parse :: String -> Result [Clazz ()]
parse = parseCool . lexerStart
}
