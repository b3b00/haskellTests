module Ast where 

{-
	Abstract Syntax Tree
-}
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
              deriving (Show)

data Stmt = Seq [Stmt]
          | AssignA String AExpr
          | AssignB String BExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Print AExpr
          | Skip
            deriving (Show)