module Parser where
import Ast
import Data.List  
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Debug.Trace (trace)


{-
   Lexer
-}    

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "print"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not", "=="
                                     ]
           }

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
semi       = Token.semi       lexer -- parses a semicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace      

lexer = Token.makeTokenParser languageDef

{-
	Parser
-}

-- statements

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> sequenceOfStmt

sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list


statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt
           <|> printStmt

printStmt :: Parser Stmt
printStmt =
  do reserved "print"
     expr <- expression
     pos <- getPosition
     return $ Print pos expr 
    
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     pos <- getPosition
     cond  <- expression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If pos cond stmt1 stmt2 
 
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     pos <- getPosition
     cond <- expression
     reserved "do"
     stmt <- statement
     return $ While pos cond stmt 



assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     pos <- getPosition     
     reservedOp ":="
     expr <- expression
     return $  Assign pos var expr 
 
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip


-- expressions 

expression :: Parser Expr
expression = buildExpressionParser operators term
 


{-operators = [ [Prefix (reservedOp "-"   >> return (Neg ))          ]
             , [Infix  (reservedOp "*"   >> return (Binary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (Binary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Binary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Binary Substract)) AssocLeft],
             [Prefix (reservedOp "not" >> return (Not             ))          ]             
             , [Infix  (reservedOp "and" >> return (Binary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (Binary Or      )) AssocLeft
              ],
              [Infix  (reservedOp "<" >> return (Binary Lesser     )) AssocLeft,
               Infix  (reservedOp ">"  >> return (Binary Greater      )) AssocLeft,
               Infix  (reservedOp "=="  >> return (Binary Equals      )) AssocLeft
              ]]-}

operators = [ [Prefix (Neg <$> getPosition <* reservedOp "-")]
             , [Infix  (Binary Multiply <$> getPosition <* reservedOp "*") AssocLeft,
                Infix  (Binary Divide <$> getPosition <* reservedOp "/") AssocLeft]
             , [Infix  (Binary Add <$> getPosition <* reservedOp "+") AssocLeft,
                Infix  (Binary Substract <$> getPosition <* reservedOp "-") AssocLeft],
             [Prefix (Not <$> getPosition <* reservedOp "not")]           
             , [Infix (Binary And <$> getPosition <* reservedOp "and") AssocLeft,
                Infix (Binary Or <$> getPosition <* reservedOp "or") AssocLeft
              ],
              [Infix (Binary Lesser <$> getPosition <* reservedOp "<") AssocLeft,
               Infix  (Binary Greater <$> getPosition <* reservedOp ">") AssocLeft,
               Infix  (Binary Equals <$> getPosition <* reservedOp "==") AssocLeft
              ]]              
 

term =  parens expression
     <|> do id <- identifier
            pos <- getPosition
            return $ Var pos id 
     <|> do v <- integer 
            pos  <- getPosition 
            return $ IntConst pos v 
     <|> do 
          reserved "true" 
          pos <- getPosition 
          return $ (BoolConst pos True )
     <|> do 
          reserved "false" 
          pos <- getPosition 
          return $ (BoolConst pos True )

    


    
         


{-
	EntryPoint
-}         

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r
    
    
parseFile :: String -> IO Stmt
parseFile file =
  do program  <- readFile file
     case parse whileParser "" program of
       Left e  -> print e >> fail "parse error"
       Right r -> return r