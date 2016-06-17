module Runner where 
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
14  JMP     inconditional jump : move code pointer to next bytecode avlue
15  JIF      conditional jump move code pointer to next bytecode avlue if top stack value
16  PRT     print top most value
17 NOOP     no operation (id function)
-}

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs     
     | otherwise = x:(replaceNth (n-1) newVal xs)

setInHeap :: Int -> StackValue -> [StackValue] -> [StackValue]
setInHeap address value heap = replaceNth address value heap


{- ********************************
    
    INT OPERATIONS

   ******************************** -}    

aBinOpFunction :: Int -> (Int -> Int -> Int)
aBinOpFunction op = case op of
    4 -> \x y -> x + y
    5 -> \x y -> x - y
    6 -> \x y -> x * y
    7 -> \x y -> x `quot` y



aBinaryOp :: Machine -> Int -> (Int -> Int -> Int) -> Machine
aBinaryOp machine opcode op = 
    let popped = (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x = (snd (fst popped)) in 
                let y =  (fst (fst popped)) in 
                    let res = ( op (getIntValue (x)) (getIntValue (y)) ) in
                        let appendedStack =  pushValue newStack (IntVal res) in
                             Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine) 


{- ********************************
    
    BOOL OPERATIONS

   ******************************** -}    

rBinOpFunction :: Int -> (Int -> Int -> Bool)
rBinOpFunction op = case op of
    11 -> \x y -> x == y
    12 -> \x y -> x > y
    13 -> \x y -> x < y
    
bBinOpFunction :: Int -> (Bool -> Bool -> Bool)
bBinOpFunction op = case op of
    9 -> \x y -> x && y
    10 -> \x y -> x ||  y
    


bBinaryOp :: Machine -> Int -> (Bool -> Bool -> Bool) -> Machine
bBinaryOp machine opcode op = 
    let popped = (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x =  (snd (fst popped)) in 
                let y =  (fst (fst popped)) in 
                    let res = ( op (getBoolValue (x)) (getBoolValue (y)) ) in
                        let appendedStack =  pushValue newStack (BoolVal res) in
                             Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine) 



bNotOp :: Machine -> Machine
bNotOp machine =
    let popped = popValue (stack machine) in
        let newStack = (snd popped)  in
            let v = (getBoolValue (fst popped)) in 
                let appendedStack =  pushValue newStack (BoolVal v) in                     
                    (Machine ((pointer machine ) +1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))



rBinaryOp :: Machine -> Int -> (Int -> Int -> Bool) -> Machine
rBinaryOp machine opcode op = 
    let popped = {-trace ("binary op :: "++(show opcode)++" with "++(show machine))-} (doublePopValue (stack machine)) in
        let newStack = {-trace ("pop :: "++(show (popped)))-} (snd popped) in
            let x = {-trace ("poping x")-} (snd (fst popped)) in 
                let y = {-trace ("poping y")-} (fst (fst popped)) in 
                    let res = {-trace ("computing operation")-} ( op (getIntValue (x)) (getIntValue (y)) ) in
                        let appendedStack = trace ("result :: "++(show res)) pushValue newStack (BoolVal res) in
                             Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine)   





{- ********************************
    
    OTHER OPCODES

   ******************************** -}    

movOp :: Machine -> Machine
 
movOp machine = 
    let  debug =  1 in  
        let popped =  popValue (stack machine) in
            let  value =  (fst popped) in
                let bcaddr = (opCodeRel machine 1) in
                        let newheap =  setInHeap (bcaddr) value (heap machine) in
                            let newStack = (snd popped) in
                                Machine ((pointer machine)+2) (bytecode machine) newStack newheap (heapAddresses machine)

pushOp :: Machine ->  Machine
pushOp machine = 
    let debug =  1 in 
        let k =  ((pointer machine)+1) in
            let addr =  ((bytecode machine) !! (k)) in -- trace ("push op get head address... heap["++(show (k-1))++"] ")  in                        
                let value = (heap machine) !! (addr-1)  in
                    let newStack =  pushValue (stack machine) value in
                        (Machine ((pointer machine)+2) (bytecode machine) newStack (heap machine) (heapAddresses machine))

printOp :: Machine -> Machine
printOp machine =
    let popped = popValue (stack machine) in 
        let newStack = (snd popped) in
            let value = (fst popped) in
                trace ("PRINT "++(show value)) (Machine ((pointer machine) +1) (bytecode machine) newStack (heap machine) (heapAddresses machine))

noOp :: Machine -> Machine
noOp machine = machine


{- ********************************
    
    ENTRY POINT

   ******************************** -}    

runIt :: Machine -> Machine

runIt machine 
    | (pointer machine) >= (length (bytecode machine)) = machine
    | otherwise = runIt' machine


memCode :: Int -> String
memCode mc = case mc of
    1 -> "PUSH"
    3 -> "MOV"
    4 -> "ADD"
    5 -> "SUB"
    6 -> "MUL"
    7 -> "DIV"
    8 -> "NOT"
    9 -> "AND"
    10 -> "OR"
    11 -> "EQ"
    12 -> "GT"
    13 -> "LT"
    _ -> "TODO"

genericOpCodeFunctions :: Int -> (Machine -> Machine)
genericOpCodeFunctions opcode = case opcode of 
    1 -> pushOp
    3 -> movOp
    8 -> bNotOp
    16 -> printOp
    17 -> noOp
    _ -> noOp



runIt' :: Machine -> Machine        
runIt' machine 
    | (opCodeIn machine [1,3,8,16,17]) =  
        let opCodeFunction = (genericOpCodeFunctions (opCode machine)) in
            let opIt= opCodeFunction machine  in          
                runIt opIt         
    | (opCodeIn machine [4..7]) =             
             let opIt = (aBinaryOp machine  (opCode machine) (aBinOpFunction (opCode machine))) in
                runIt opIt 
    | (opCodeIn machine [9,10]) =             
             let opIt = (bBinaryOp machine  (opCode machine) (bBinOpFunction (opCode machine))) in
                trace ((memCode (opCode machine))++" @"++(show (pointer machine))) runIt opIt
    | (opCodeIn machine [11,13]) =             
             let opIt = (rBinaryOp machine  (opCode machine) (rBinOpFunction (opCode machine))) in
                runIt opIt     
    | otherwise = Machine (-1) [] [] [] [] 


runMachine :: Machine -> Machine  
runMachine machine =  runIt machine
    
    
