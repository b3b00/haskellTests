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





compileAst :: Stmt -> Machine -> Machine
compileAst stmt machine = compileStmt stmt machine


compileAExpr :: AExpr ->  Machine -> Machine        
compileAExpr expr machine = case expr of
    IntConst i ->  Machine 0 (bytecode machine++[1, (length( heap machine))]) (stack machine) (heap machine++[IntVal (fromIntegral i)])  (heapAddresses machine)
    Var n -> Machine 0 (bytecode machine++[1,(getVariableInt n (heapAddresses machine))]) (stack machine) (heap machine) (heapAddresses machine)
    ABinary op left right -> compileAbinary expr machine
    Neg expr -> let compiledExpr = compileAExpr expr machine in
        Machine 0 ((bytecode compiledExpr)++[8]) (stack compiledExpr) (heap compiledExpr) (heapAddresses compiledExpr)
        


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
    And -> 10
    Or -> 11

compileBBinary :: BExpr -> Machine -> Machine
compileBBinary expr machine = case expr of
    BBinary op left right -> 
        let leftMachine = compileBExpr left machine in          -- 1: compile left
                    let rightMachine = compileBExpr right leftMachine in    -- 2: compile right
                        (Machine 0 ((bytecode rightMachine)++[(bBinOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))
    otherwise -> error "incorrect execution path !"
    
rBinOpCode :: RBinOp -> Int
rBinOpCode op = case op of
    Greater -> 13
    Less  -> 14
    Equals -> 12

compileRBinary :: RBinOp -> AExpr -> AExpr -> Machine -> Machine
compileRBinary op left right machine = 
            let leftMachine = compileAExpr left machine in          -- 1: compile left
                let rightMachine = compileAExpr right leftMachine in    -- 2: compile right
                    (Machine 0 ((bytecode rightMachine)++[(rBinOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))

compileBExpr :: BExpr ->  Machine -> Machine        
compileBExpr expr machine = case expr of
    BoolConst b -> Machine 0 (bytecode machine++[1, (length( heap machine))]) (stack machine) (heap machine++[BoolVal b]) (heapAddresses machine)
    Not expr -> let newMachine = compileBExpr expr machine in
            Machine 0 ((bytecode newMachine)++[9]) (stack newMachine) (heap newMachine) (heapAddresses newMachine)
    BBinary op e1 e2 -> compileBBinary expr machine
    RBinary rop e1 e2 -> compileRBinary rop e1 e2 machine 

compileSequence :: [Stmt] -> Machine -> Machine
compileSequence [] machine = machine
compileSequence (h:t) machine = compileSequence t (compileStmt h machine)      


compileAssignByteCode :: String -> Machine -> Machine
compileAssignByteCode name machine = let newMachine = setAddressForVariableInHeap name machine in 
        let address = getVariableAddress name newMachine in 
            Machine 0 ((bytecode newMachine)++[3,address]) (stack newMachine) (heap newMachine) (heapAddresses newMachine) 


replace :: Int -> Int -> [Int] -> [Int]
replace index value array =  (take (index-1) array) ++ [value] ++ (drop index array)

setJMPAddres :: Int -> Int -> Machine -> Machine
setJMPAddres destination opCodeAddress machine = 
    let replacedBC = (replace opCodeAddress destination (bytecode machine)) in
         Machine 0  replacedBC (stack machine) (heap machine) (heapAddresses machine)

compileIfThenElse :: BExpr -> Stmt -> Stmt -> Machine -> Machine
compileIfThenElse cond ifStmt elseStmt machine = 
    let compiledCond = compileBExpr cond machine in
        -- add JNt/JF (17) / reserve an emptycode (and store index)
        let compiledConditionalJump = (Machine  0 ((bytecode compiledCond)++[17,0]) (stack compiledCond) (heap compiledCond) (heapAddresses compiledCond)) in
            let jntAddress = (length (bytecode compiledConditionalJump) ) in 
                let comiledIfblock =  compileStmt ifStmt compiledConditionalJump in
                    let compiledJMP = (Machine  0 ((bytecode comiledIfblock)++[15,0]) (stack comiledIfblock) (heap comiledIfblock) (heapAddresses comiledIfblock)) in
                        let jmpAdress = (length (bytecode compiledJMP)) in 
                            let compiledElseBlock =  compileStmt elseStmt compiledJMP in
                                let uncondjumpaddr =  (length (bytecode compiledElseBlock)) in                                                                
                                    let replacedUncodJMP = setJMPAddres uncondjumpaddr jmpAdress compiledElseBlock in                                         
                                        --replacedUncodJMP
                                        let replacedJNT = setJMPAddres jmpAdress jntAddress replacedUncodJMP in 
                                            replacedJNT
                                
compileWhileStatement :: BExpr -> Stmt -> Machine -> Machine
compileWhileStatement cond block machine = 
    let loopStartAddress =  (length (bytecode machine)) in
        let compiledCond = compileBExpr cond machine in 
            let finishJump =  Machine 0  ((bytecode compiledCond)++[17,0]) (stack compiledCond) (heap compiledCond) (heapAddresses compiledCond)  in
                let jntAddress = (length (bytecode finishJump)) in
                    let compiledLoopStmt = compileStmt block finishJump in
                        let loopback = Machine 0 ((bytecode compiledLoopStmt)++[15,loopStartAddress]) (stack compiledLoopStmt) (heap compiledLoopStmt) (heapAddresses compiledLoopStmt) in 
                            let finishLoopAddress = length (bytecode loopback) in
                                let replacedJNT =  setJMPAddres finishLoopAddress jntAddress loopback in 
                                    replacedJNT
                                --compiledLoopStmt

        
        -- compile ifstmt and store last index+1 + ajout JMP + reserve emptycode and store index) => get last index and store it afeter JNT
            --compile elseStmt => get last index and store it after JMP
              --return machine


compileStmt :: Stmt -> Machine -> Machine
compileStmt stmt machine = case stmt of 
    AssignA name expr -> 
        let compiledExpr =  compileAExpr expr machine in            
                compileAssignByteCode name compiledExpr            
    AssignB name expr -> 
        let compiledExpr =   compileBExpr expr machine in
                compileAssignByteCode name compiledExpr
    Skip ->   Machine 0 ((bytecode machine)++[19]) (stack machine) (heap machine) (heapAddresses machine)
    If conf ifStmt elseStmt ->  compileIfThenElse conf ifStmt elseStmt machine
    Print expr -> let previous = compileAExpr expr machine in
         Machine 0 ((bytecode previous)++[18]) (stack previous) (heap previous) (heapAddresses previous)
    Seq stmts ->  compileSequence stmts machine
    While cond block -> compileWhileStatement cond block machine