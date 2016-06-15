module Compiler where 
import System.IO
import Control.Monad
import Ast
import Machine
import Stack
import Assoc
import Debug.Trace (trace)
{-

todo : define bytecode / assembly

1   PUSH    push a value on stack (next bc value is value address in heap)
2   POP  pop a value from stack
3   MOV     pop value from stack and store in heap at next bytecode address
4   ADD  add the two topmost values and push result 
5   SUB  substract the two topmost values and push result
6   MUL  multiply the two topmost values and push result
7   DIV  divide the two topmost values and push result
8   EQ  push true if two top values are equals
9   GT  push true if n-1 > n
10  LT  push true if n-1 < n
11  JMP  move code pointer to next bytecode avlue 
12  JT   move code pointer to next bytecode avlue if top stack value is True (true == 1)
13  JNT      move code pointer to next bytecode avlue if top stack value is Not True  (False == 0)
14  PRT  print top most value     

-}





compileAst :: Stmt -> Machine -> Machine
compileAst stmt machine = case stmt of 
    Seq stmts -> trace "compStm seq" Machine [0,1,2,3,4,5,6] [] [] []
    While cond stm -> trace "compstm wwhile" Machine [6,7,8] [] [] []
    If cond thenStmt elseStmt -> trace "compstm if" Machine [9,10,11,12] [] [] []
    Print expr -> trace "compstm print" Machine [13] [] [] []
    Skip -> trace "compstm skip" Machine [] [] [] []
    _ -> trace "compstm ont know" Machine [] [] [] []


compileAExpr :: AExpr ->  Machine -> Machine        
compileAExpr expr machine = case expr of
    IntConst i -> Machine (bytecode machine++[1, (length( heap machine)) + 1]) (stack machine) (heap machine++[IntVal (fromIntegral i)])  (heapAddresses machine)
    Var n -> Machine (bytecode machine++[1,(getVariableInt n (heapAddresses machine))]) (stack machine) (heap machine) (addOrReplaceInt n  (length (heap machine)) (heapAddresses machine))
    ABinary op left right -> compileAbinary expr machine

aBinOpOpCode :: ABinOp -> Int
aBinOpOpCode op = case op of
    Add -> 4
    Substract -> 5
    Multiply -> 6
    Divide -> 7
 


compileAbinary :: AExpr -> Machine -> Machine -- AExpr is a ABinary
compileAbinary expr machine = case expr of 
    ABinary op left right -> 
        let leftMachine = compileAExpr left machine in          -- 1: compile left
            let rightMachine = compileAExpr right leftMachine in    -- 2: compile right
                (Machine ((bytecode rightMachine)++[(aBinOpOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))
    otherwise -> error "incorrect execution path !"


-- 2: compile right
-- 3: add aBinOpOpCode op in bytecode list





    

compileBExpr :: BExpr ->  Machine -> Machine        
compileBExpr expr machine = case expr of
    BoolConst b -> Machine (bytecode machine++[1, (length( heap machine)) + 1]) (stack machine) (heap machine++[BoolVal b]) (heapAddresses machine)

compileStmt :: Stmt -> Machine -> Machine
compileStmt stmt machine = case stmt of 
    Assign name expr -> Machine (bytecode (compileAExpr expr machine)++[3, (length( heap (compileAExpr expr machine))) + 1]) (stack (compileAExpr expr machine)) (heap (compileAExpr expr machine)++[NullVal]) (addOrReplaceInt name ( (length (heap (compileAExpr expr machine)))) (heapAddresses (compileAExpr expr machine) )) -- on ajoute NulVal dans le heap juste pour réserver la place