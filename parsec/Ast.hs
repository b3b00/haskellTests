module Ast where 

import Text.Parsec(SourcePos)

{-
	Abstract Syntax Tree
-}

data Expr = BoolConst Bool SourcePos
  | IntConst Integer SourcePos
  | Var String SourcePos
  | Neg Expr
  | Not Expr
  | Binary BinOp Expr Expr
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
          | Assign String Expr SourcePos         
          | If Expr Stmt Stmt SourcePos
          | While Expr Stmt SourcePos
          | Print Expr SourcePos
          | Skip 
            deriving (Show)

