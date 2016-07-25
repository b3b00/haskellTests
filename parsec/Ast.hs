module Ast where 

import Text.Parsec(SourcePos)

{-
	Abstract Syntax Tree
-}

data Expr = BoolConst SourcePos Bool 
  | IntConst SourcePos Integer
  | StringConstSourcePos String
  | Var SourcePos String 
  | Neg SourcePos Expr 
  | Not SourcePos Expr 
  | Binary BinOp SourcePos Expr Expr
    deriving (Show)


data BinOp = Add
  | Substract
  | Multiply
  | Divide
  | And
  | Or
  | Greater
  | Lesser
  | Equals
    deriving (Show)

data Stmt = Seq [Stmt]
          | Assign SourcePos String Expr          
          | If SourcePos Expr Stmt Stmt 
          | While SourcePos Expr Stmt 
          | Print SourcePos Expr 
          | Skip 
            deriving (Show)

