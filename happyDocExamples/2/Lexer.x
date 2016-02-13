{
module Lexer where
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+       { \s -> TokenWS }
  let         { \s -> TokenLet }
  in          { \s -> TokenIn }
  $digit+       { \s -> TokenInt (read s) }
  [\=\+\-\*\/\(\)]      { \s -> symbol (head s) }
  $alpha [$alpha $digit \_ \']*   { \s -> TokenVar s }
{ 

symbol :: Char -> Token
symbol '=' = TokenEq
symbol '+' = TokenPlus
symbol '-' = TokenMinus
symbol '*' = TokenTimes
symbol '/' = TokenDiv
symbol '(' = TokenOB
symbol ')' = TokenCB

{- cleanTokens :: [Token] -> [Token]
 cleanTokens toks = case toks of
  [] -> []
  [TokenWS:xs] -> cleanTokens xs
  [x:xs] -> [x:(cleanTokens xs)] 

cleanTokens toks 
  | ((length toks) == 0) = []
  | (head(toks) == TokenWS) = cleanTokens (tail toks)
  | otherwise = [(head toks):(cleanTokens( tail toks))]  -}
 
scan :: String -> [Token] 
scan str = filter (\x -> x /= TokenWS) (alexScanTokens str)


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
      | TokenWS
      | TEOF
  deriving (Eq,Show)


}