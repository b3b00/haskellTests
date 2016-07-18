module SemanticChecker where 
import System.IO
import Control.Monad
import Ast
import Machine
import Stack
import Assoc
import Debug.Trace (trace)
{-

todo : define bytecode / assembly

1   PUSH    push a value on stack (next bc value is value address)
2   POP pop a value from stack
3   MOV pop value from stack and store in heap at next bytecode
4   ADD add the two topmost values and push result
5   SUB substract the two topmost values and push result
6   MUL multiply the two topmost values and push result
7   DIV divide the two topmost values and push result
8   NEG negate top most value on stack
9   NOT push true if top most is false, false otherwise
10  AND push and value of 2 topmost
11  OR  push or value of 2 topmost
12  EQ  push true if two top values are equals
13  GT  push true if n-1 > n
14  LT  push true if n-1 < n
15  JMP move code pointer to next bytecode avlue
16  JT  move code pointer to next bytecode avlue if top stack value
17  JNT move code pointer to next bytecode avlue if top stack value
18  PRT print top most value

-}

data ExprType = IntExpr
  | BoolExpr
  | UnknownExpr
  | ErrorExpr
  deriving (Show, Eq)


-- | vérifie la correction sémantique
-- retourne  une liste d'erreur. Si le programme est correct retourne une liste vide.
semanticCheck :: Stmt -> [String]
semanticCheck ast = []


binaryCompatibilty :: (BinOp, ExprType, ExprType) -> ExprType
binaryCompatibilty op = case op  of
    (Add, IntExpr, IntExpr) -> IntExpr
    (Substract, IntExpr, IntExpr) -> IntExpr
    (Multiply, IntExpr, IntExpr) -> IntExpr
    (Divide, IntExpr, IntExpr) -> IntExpr
    (And, BoolExpr, BoolExpr) -> IntExpr
    (Or, BoolExpr, BoolExpr) -> IntExpr
    (Lesser, IntExpr, IntExpr) -> IntExpr
    (Greater, IntExpr, IntExpr) -> IntExpr
    (Equals, IntExpr, IntExpr) -> IntExpr
    otherwise -> ErrorExpr


getBinaryExprType :: BinOp -> Expr -> Expr -> ExprType
getBinaryExprType op left right =
    case op of
        Add -> let rt = getExprType right in
                let lt = getExprType left in
                    if rt == IntExpr && lt == IntExpr then IntExpr else ErrorExpr

getExprType :: Expr -> ExprType
getExprType expr = case expr of 
    IntConst i ->  IntExpr
    BoolConst b -> BoolExpr
    Var n -> UnknownExpr
    Binary op left right -> binaryCompatibilty (op,(getExprType left),(getExprType right))
    Neg exprN ->  let rightType = (trace ("testing - "++(show exprN))) getExprType exprN in
        if rightType == IntExpr then IntExpr else ErrorExpr        
    Not exprN -> let rightType = (trace ("testing not "++(show exprN))) getExprType exprN in
        if (rightType == BoolExpr) then BoolExpr else ErrorExpr
            



