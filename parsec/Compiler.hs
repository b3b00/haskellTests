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


compileExpr :: Expr ->  Machine -> Machine        
compileExpr expr machine = case expr of
    IntConst ops i  ->  Machine 0 (bytecode machine++[1, (length( heap machine))]) (stack machine) (heap machine++[IntVal (fromIntegral i)])  (heapAddresses machine)
    BoolConst pos b  -> Machine 0 (bytecode machine++[1, (length( heap machine))]) (stack machine) (heap machine++[BoolVal b]) (heapAddresses machine)
    StringConst pos s  -> Machine 0 (bytecode machine++[1, (length( heap machine))]) (stack machine) (heap machine++[StringVal s]) (heapAddresses machine)
    Var pos n -> Machine 0 (bytecode machine++[1,(getVariableInt n (heapAddresses machine))]) (stack machine) (heap machine) (heapAddresses machine)
    Binary op pos left right  -> compileBinary expr machine
    Neg pos expr -> let compiledExpr = compileExpr expr machine in
        Machine 0 ((bytecode compiledExpr)++[54]) (stack compiledExpr) (heap compiledExpr) (heapAddresses compiledExpr)
    Not pos expr -> let newMachine = compileExpr expr machine in
        Machine 0 ((bytecode newMachine)++[55]) (stack newMachine) (heap newMachine) (heapAddresses newMachine)        
        



binOpCode :: BinOp -> Int
binOpCode op = case op of
    Add -> 59
    Substract -> 51
    Multiply -> 52
    Divide -> 53
    And -> 56
    Or -> 57
    Greater -> 59
    Lesser  -> 60
    Equals -> 58
    Concat -> 61
 


compileBinary :: Expr -> Machine -> Machine 
compileBinary expr machine = case expr of 
    Binary op pos left right -> 
        let leftMachine = compileExpr left machine in          -- 1: compile left
            let rightMachine = compileExpr right leftMachine in    -- 2: compile right
                (Machine 0 ((bytecode rightMachine)++[(binOpCode op)]) (stack rightMachine) (heap rightMachine) (heapAddresses rightMachine))
    otherwise -> error ("incorrect execution path ! "++(show expr))



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

compileIfThenElse :: Expr -> Stmt -> Stmt -> Machine -> Machine
compileIfThenElse cond ifStmt elseStmt machine = 
    let compiledCond = compileExpr cond machine in
        -- add JNt/JF (12) / reserve an emptycode (and store index)
        let compiledConditionalJump = (Machine  0 ((bytecode compiledCond)++[12,0]) (stack compiledCond) (heap compiledCond) (heapAddresses compiledCond)) in
            let jntAddress = (length (bytecode compiledConditionalJump) ) in 
                let comiledIfblock =  compileStmt ifStmt compiledConditionalJump in
                    let compiledJMP = (Machine  0 ((bytecode comiledIfblock)++[10,0]) (stack comiledIfblock) (heap comiledIfblock) (heapAddresses comiledIfblock)) in
                        let jmpAdress = (length (bytecode compiledJMP)) in 
                            let compiledElseBlock =  compileStmt elseStmt compiledJMP in
                                let uncondjumpaddr =  (length (bytecode compiledElseBlock)) in                                                                
                                    let replacedUncodJMP = setJMPAddres uncondjumpaddr jmpAdress compiledElseBlock in                                         
                                        --replacedUncodJMP
                                        let replacedJNT = setJMPAddres jmpAdress jntAddress replacedUncodJMP in 
                                            replacedJNT
                                
compileWhileStatement :: Expr -> Stmt -> Machine -> Machine
compileWhileStatement cond block machine = 
    let loopStartAddress =  (length (bytecode machine)) in
        let compiledCond = compileExpr cond machine in 
            let finishJump =  Machine 0  ((bytecode compiledCond)++[12,0]) (stack compiledCond) (heap compiledCond) (heapAddresses compiledCond)  in
                let jntAddress = (length (bytecode finishJump)) in
                    let compiledLoopStmt = compileStmt block finishJump in
                        let loopback = Machine 0 ((bytecode compiledLoopStmt)++[10,loopStartAddress]) (stack compiledLoopStmt) (heap compiledLoopStmt) (heapAddresses compiledLoopStmt) in 
                            let finishLoopAddress = length (bytecode loopback) in
                                let replacedJNT =  setJMPAddres finishLoopAddress jntAddress loopback in 
                                    replacedJNT
                                --compiledLoopStmt

        
        -- compile ifstmt and store last index+1 + ajout JMP + reserve emptycode and store index) => get last index and store it afeter JNT
            --compile elseStmt => get last index and store it after JMP
              --return machine


compileStmt :: Stmt -> Machine -> Machine
compileStmt stmt machine = case stmt of 
    Assign pos name expr -> 
        let compiledExpr =   compileExpr expr machine in            
                compileAssignByteCode name compiledExpr                
    Skip ->   Machine 0 ((bytecode machine)++[21]) (stack machine) (heap machine) (heapAddresses machine)
    If pos conf ifStmt elseStmt  ->   compileIfThenElse conf ifStmt elseStmt machine
    Print pos expr -> let previous =  compileExpr expr machine in
         Machine 0 ((bytecode previous)++[20]) (stack previous) (heap previous) (heapAddresses previous)
    Seq stmts ->  compileSequence stmts machine
    While pos cond block ->  compileWhileStatement cond block machine