-- Grammar.y -*- mode: haskell -*-
{
module Parser where
import Scanner 
import Debug.Trace
}

%name parse
%tokentype { Token }


%token 
      let             { TokenLet }
      in              { TokenIn }
      int             { TokenInt $$ }
      var             { TokenVar $$ }
      '='             { TokenEq }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '/'             { TokenDiv }
      '('             { TokenOB }
      ')'             { TokenCB }


%%

Exp   : let var '=' Exp in Exp  { \p -> traceShow ("LET :: "++(show p)) $6 (($2,$4 p):p) }
      | Exp1                    { $1 }

Exp1  : Exp1 '+' Term           { \p -> traceShow ("PLUS :: "++(show p)) $1 p + $3 p }
      | Exp1 '-' Term           { \p -> traceShow ("MINUS :: "++(show p))$1 p - $3 p }
      | Term                    { $1 }

Term  : Term '*' Factor         { \p -> traceShow ("TIMES :: "++(show p))$1 p * $3 p }
      | Term '/' Factor         { \p -> traceShow ("DIV :: "++(show p))$1 p `div` $3 p }
      | Factor                  { $1 }

Factor			  
      : int                     { \p -> $1 }
      | var                     { \p -> case lookup $1 p of
	                                    Nothing -> error "no var"
					    Just i  -> i }
      | '(' Exp ')'             { $2 }
	  
{
happyError a = error "error ! so baad!"
}	  
	
