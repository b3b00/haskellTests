module Ast where 

{-
	Abstract Syntax Tree
-}

data Expr = BoolConst Bool
  | IntConst Integer
  | Var String
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
          | Assign String Expr          
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Print Expr
          | Skip
            deriving (Show)

