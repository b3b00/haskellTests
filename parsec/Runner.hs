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
16  JT  move code pointer to next bytecode avlue if top stack value is True
17  JNT move code pointer to next bytecode avlue if top stack value is False
18  PRT print top most value

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


aNegOp :: Machine -> Machine
aNegOp machine =
    let popped =  popValue (stack machine) in
        let newStack = (snd popped)  in
            let v =  (getIntValue (fst popped)) in            
                let negV = 0 - v in    
                    let appendedStack = (pushValue newStack (IntVal negV)) in                     
                         (Machine ((pointer machine ) +1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))

{- ********************************
    
    BOOL OPERATIONS

   ******************************** -}    

rBinOpFunction :: Int -> (Int -> Int -> Bool)
rBinOpFunction op = case op of
    12 -> \x y -> x == y
    13 -> \x y -> x > y
    14 -> \x y -> x < y
    
bBinOpFunction :: Int -> (Bool -> Bool -> Bool)
bBinOpFunction op = case op of
    10 -> \x y -> x && y
    11 -> \x y -> x ||  y
    


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
    let popped =  popValue (stack machine) in
        let newStack =  (snd popped)  in
            let v = (getBoolValue (fst popped)) in 
                let appendedStack =   pushValue newStack (BoolVal v) in                     
                     (Machine ((pointer machine ) +1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))



rBinaryOp :: Machine -> Int -> (Int -> Int -> Bool) -> Machine
rBinaryOp machine opcode op = 
    let popped =  (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x =  (snd (fst popped)) in 
                let y = (fst (fst popped)) in 
                    let res =  ( op (getIntValue (x)) (getIntValue (y)) ) in
                        let appendedStack =  pushValue newStack (BoolVal res) in
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
            let addr =  ((bytecode machine) !! (k)) in 
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
    8 -> "NEG"
    9 -> "NOT"
    10 -> "AND"
    11 -> "OR"
    12 -> "EQ"
    13 -> "GT"
    14 -> "LT"
    18 -> "PRINT"
    19 -> "NOOP"
    _ -> "TODO"

genericOpCodeFunctions :: Int -> (Machine -> Machine)
genericOpCodeFunctions opcode = case opcode of 
    1 -> pushOp
    3 -> movOp
    8 -> aNegOp
    9 -> bNotOp
    18 -> printOp
    19 -> noOp
    _ -> noOp



runIt' :: Machine -> Machine        
runIt' machine 
    | (opCodeIn machine [1,3,8,9,16,17]) =  
        let opCodeFunction = (genericOpCodeFunctions (opCode machine)) in
            let opIt= opCodeFunction machine  in          
                runIt opIt         
    | (opCodeIn machine [4..7]) =             
             let opIt = (aBinaryOp machine  (opCode machine) (aBinOpFunction (opCode machine))) in
                runIt opIt     
    | (opCodeIn machine [10,11]) =             
             let opIt = (bBinaryOp machine  (opCode machine) (bBinOpFunction (opCode machine))) in
                 runIt opIt
    | (opCodeIn machine [12,14]) =             
             let opIt = (rBinaryOp machine  (opCode machine) (rBinOpFunction (opCode machine))) in
                runIt opIt     
    | otherwise = Machine (-1) [] [] [] [] 


runMachine :: Machine -> Machine  
runMachine machine =  runIt machine
    
    
