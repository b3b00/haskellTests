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

1   PUSH    push a value on stack (next bc value is value address in
2   POP     pop a value from stack
3   MOV     pop value from stack and store in heap at next bytecode
4   ADD     add the two topmost values and push result
5   SUB     substract the two topmost values and push result
6   MUL     multiply the two topmost values and push result
7   DIV     divide the two topmost values and push result
8   NOT     push true if top most is false, false otherwise
9   AND     push and value of 2 topmost
10  OR      push or value of 2 topmost
11  EQ      push true if two top values are equals
12  GT      push true if n-1 > n
13  LT      push true if n-1 < n
14  JMP     move code pointer to next bytecode avlue
15  JT      move code pointer to next bytecode avlue if top stack value
16  JNT     move code pointer to next bytecode avlue if top stack v
17  PRT     print top most value
  

-}





compileAst :: Stmt -> Machine -> Machine
compileAst stmt machine = compileStmt stmt machine


compileAExpr :: AExpr ->  Machine -> Machine        
compileAExpr expr machine = case expr of
    IntConst i -> Machine 0 (bytecode machine++[1, (length( heap machine)) + 1]) (stack machine) (heap machine++[IntVal (fromIntegral i)])  (heapAddresses machine)
    Var n -> Machine 0 (bytecode machine++[1,(getVariableInt n (heapAddresses machine))]) (stack machine) (heap machine) (addOrReplaceInt n  (length (heap machine)) (heapAddresses machine))
    ABinary op left right -> compileAbinary expr machine

aBinOpCode :: ABinOp -> Int
aBinOpCode op = case op of
    Add -> 4
    Substract -> 5
    Multiply -> 6
    Divide -> 7
 


compileAbinary :: AExpr -> Machine -> Machine -- AExpr is a ABinary
compileAbinary expr machine = case expr of 
    ABinary op left right -> 
        let leftMachine = compileAExpr left machine in          -- 1: compile left
            let rightMachine = compileAExpr right leftMachine in    -- 2: compile right
                (Machine 0 ((bytecode rightMachine)++[(aBinOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))
    otherwise -> error "incorrect execution path !"


-- 2: compile right
-- 3: add aBinOpOpCode op in bytecode list


bBinOpCode :: BBinOp -> Int
bBinOpCode op = case op of
    And -> 9
    Or -> 10

compileBBinary :: BExpr -> Machine -> Machine
compileBBinary expr machine = case expr of
    BBinary op left right -> 
        let leftMachine = compileBExpr left machine in          -- 1: compile left
                    let rightMachine = compileBExpr right leftMachine in    -- 2: compile right
                        (Machine 0 ((bytecode rightMachine)++[(bBinOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))
    otherwise -> error "incorrect execution path !"
    
rBinOpCode :: RBinOp -> Int
rBinOpCode op = case op of
    Greater -> 12
    Less  -> 13
    Equals -> 11

compileRBinary :: RBinOp -> AExpr -> AExpr -> Machine -> Machine
compileRBinary op left right machine = 
            let leftMachine = compileAExpr left machine in          -- 1: compile left
                let rightMachine = compileAExpr right leftMachine in    -- 2: compile right
                    (Machine 0 ((bytecode rightMachine)++[(rBinOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))

compileBExpr :: BExpr ->  Machine -> Machine        
compileBExpr expr machine = case expr of
    BoolConst b -> Machine 0 (bytecode machine++[1, (length( heap machine)) + 1]) (stack machine) (heap machine++[BoolVal b]) (heapAddresses machine)
    Not expr -> let newMachine = compileBExpr expr machine in
            Machine 0 ((bytecode newMachine)++[8]) (stack newMachine) (heap newMachine) (heapAddresses newMachine)
    BBinary op e1 e2 -> compileBBinary expr machine
    RBinary rop e1 e2 -> compileRBinary rop e1 e2 machine 

compileSequence :: [Stmt] -> Machine -> Machine
compileSequence [] machine = machine
compileSequence (h:t) machine = compileSequence t (compileStmt h machine)      


compileAssignByteCode :: String -> Machine -> Machine
compileAssignByteCode name machine = let newMachine = setAddressForVariableInHeap name machine in 
        let address = getVariableAddress name newMachine in 
            Machine 0 ((bytecode newMachine)++[3,address]) (stack newMachine) (heap newMachine) (heapAddresses newMachine) 



compileStmt :: Stmt -> Machine -> Machine
compileStmt stmt machine = case stmt of 
    AssignA name expr -> 
        let compiledExpr = compileAExpr expr machine in            
                compileAssignByteCode name compiledExpr            
    AssignB name expr -> 
        let compiledExpr = compileBExpr expr machine in
                compileAssignByteCode name compiledExpr
    Skip -> machine       
    Seq stmts -> compileSequence stmts machine