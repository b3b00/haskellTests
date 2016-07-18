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

{-
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show)

data BBinOp = And | Or  deriving (Show)     

data RBinOp = Greater | Less | Equals deriving (Show)       

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
             deriving (Show)

data ABinOp = Add
            | Substract
            | Multiply
            | Divide
              deriving (Show)-}

data Stmt = Seq [Stmt]
          | Assign String Expr          
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Print Expr
          | Skip
            deriving (Show)