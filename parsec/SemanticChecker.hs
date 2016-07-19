module SemanticChecker where 
import System.IO
import Control.Monad
import Ast
import Machine
import Stack
import Assoc
import Debug.Trace (trace)
import Text.Parsec(SourcePos)
import Text.Parsec.Pos
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

type  CheckResult = (ExprType, [String])  


-- | vérifie la correction sémantique
-- retourne  une liste d'erreur. Si le programme est correct retourne une liste vide.
semanticCheck :: Stmt -> [String]
semanticCheck ast = []

third (_, _, x) = x
second(_,x,_) = x
first(_,x,_) = x


showPos ::  SourcePos -> String
showPos pos = (show (sourceLine pos))++", "++(show (sourceColumn pos))

binaryCompatibilty :: (BinOp, ExprType, ExprType) -> SourcePos -> CheckResult
binaryCompatibilty types pos = case types  of
    (Add, IntExpr, IntExpr) -> (IntExpr,[])
    (Substract, IntExpr, IntExpr) -> (IntExpr,[])
    (Multiply, IntExpr, IntExpr) -> (IntExpr,[])
    (Divide, IntExpr, IntExpr) -> (IntExpr,[])
    (And, BoolExpr, BoolExpr) -> (BoolExpr,[])
    (Or, BoolExpr, BoolExpr) -> (BoolExpr,[])
    (Lesser, IntExpr, IntExpr) -> (BoolExpr,[])
    (Greater, IntExpr, IntExpr) -> (BoolExpr,[])
    (Equals, IntExpr, IntExpr) -> (BoolExpr,[])
    otherwise -> (ErrorExpr,["uncompatible types ("++(show (second types))++" and "++(show (third types))++")for "++(show (first types))++" at "++(showPos pos)])


getBinaryExprType :: BinOp -> SourcePos -> Expr -> Expr -> CheckResult
getBinaryExprType op pos left right =
    case op of
        Add -> let rt = fst (getExprType right) in
                let lt = fst (getExprType left) in
                    binaryCompatibilty (op,rt,lt) pos



getExprType :: Expr -> CheckResult
getExprType expr = case expr of 
    IntConst i pos ->  (IntExpr,[])
    BoolConst b pos -> (BoolExpr,[])
    Var n pos -> (UnknownExpr,["dont known"])
    Binary op pos left right -> let lt = getExprType left in
                                    let rt = getExprType right in
                                        let compat = binaryCompatibilty (op,fst lt,fst lt) pos in
                                            (fst compat, (snd compat)++(snd lt)++(snd rt))                                            
    Neg pos exprN ->  let rightType = (trace ("testing - "++(show exprN))) getExprType exprN in
        if (fst rightType) == IntExpr then (IntExpr,[]) else (ErrorExpr ,["bad type for unary '-' operator at "++(showPos pos)]++(snd rightType))       
    Not pos exprN -> let rightType = (trace ("testing not "++(show exprN))) getExprType exprN in
        if ((fst rightType) == BoolExpr) then (BoolExpr,[]) else (ErrorExpr ,["bad type for unary 'not' operator at "++(showPos pos)]++(snd rightType)) 
            




