module Scanner where

import Data.Char

data Token = 
	  TokenLet
      | TokenIn
      | TokenInt Int
      | TokenVar String
      | TokenEq
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenOB
      | TokenCB
	  | TEOF
	deriving (Eq,Show)
	

scan :: String -> [Token]
scan [] = []
scan (c:cs) 
      | isSpace c = scan cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
scan ('=':cs) = TokenEq : scan cs
scan ('+':cs) = TokenPlus : scan cs
scan ('-':cs) = TokenMinus : scan cs
scan ('*':cs) = TokenTimes : scan cs
scan ('/':cs) = TokenDiv : scan cs
scan ('(':cs) = TokenOB : scan cs
scan (')':cs) = TokenCB : scan cs

lexNum cs = TokenInt (read num) : scan rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : scan rest
      ("in",rest)  -> TokenIn : scan rest
      (var,rest)   -> TokenVar var : scan rest
	  
	  