module Parser where
import Ast
import Data.List  
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token


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
     aexpr <- expression
     return $ Print aexpr
    
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- expression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2
 
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- expression
     reserved "do"
     stmt <- statement
     return $ While cond stmt



assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- expression
     return $ Assign var expr     
 
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip


-- expressions 

expression :: Parser Expr
expression = buildExpressionParser operators term
 


operators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
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
              ]]
 

term =  parens expression
     <|> liftM Var identifier
     <|> liftM IntConst integer
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))     
    


    
         


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