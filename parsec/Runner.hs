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
14  JMP     move code pointer to next bytecode avlue
15  JT      move code pointer to next bytecode avlue if top stack value
16  JNT     move code pointer to next bytecode avlue if top stack v
17  PRT     print top most value
-}

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

setInHeap :: Int -> StackValue -> [StackValue] -> [StackValue]
setInHeap address value heap = replaceNth address value heap


movOp :: Machine -> Machine
 
movOp machine = 
    let  debug = trace ("\nin mov op ") 1 in  
        let popped = trace "mov pop" popValue (stack machine) in
            let  value = trace ("mov v"++(show (fst popped))) (fst popped) in
                let addr = trace ("mov a val::"++(show value)) (bytecode machine) !! ((opCode machine)+1) in
                    let newheap = trace ("mov h "++(show value)++" -> @"++(show addr)) setInHeap (addr-1) value (heap machine) in
                        let newStack = (snd popped) in
                            Machine ((pointer machine)+2) (bytecode machine) newStack newheap (heapAddresses machine)




aBinOpFunction :: Int -> (Int -> Int -> Int)
aBinOpFunction op = case op of
    4 -> \x y -> x + y
    5 -> \x y -> x - y
    6 -> \x y -> x * y
    7 -> \x y -> x `quot` y

{-
1 : pop right
2 : pop left
3 : run op -> result
4 : push result

-}

getIntValue :: StackValue -> Int
getIntValue stackVal = case stackVal of
    IntVal i -> i
    BoolVal b -> if b then 1 else 0
    NullVal -> 0

aBinaryOp :: Machine -> Int -> (Int -> Int -> Int) -> Machine
aBinaryOp machine opcode op = 
    let popped = {-trace ("binary op :: "++(show opcode)++" with "++(show machine))-} (doublePopValue (stack machine)) in
        let newStack = {-trace ("pop :: "++(show (popped)))-} (snd popped) in
            let x = {-trace ("poping x")-} (snd (fst popped)) in 
                let y = {-trace ("poping y")-} (fst (fst popped)) in 
                    let res = {-trace ("computing operation")-} ( op (getIntValue (x)) (getIntValue (y)) ) in
                        let appendedStack = trace ("result :: "++(show res)) pushValue newStack (IntVal res) in
                             Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine) 


pushOp :: Machine ->  Machine
pushOp machine = 
    let debug = trace ("\nin push "++(show machine)) 1 in 
        let k = trace ("push op ip+1="++(show ((pointer machine)+1))) ((pointer machine)+1) in
            let addr = trace ("push op get head address... bytecode["++(show (k))++"]") ((bytecode machine) !! (k)) in -- trace ("push op get head address... heap["++(show (k-1))++"] ")  in                        
                let value = trace ("push op v from "++(show addr)++"heap@"++(show (addr-1))) (heap machine) !! (addr-1)  in
                    let newStack = trace ("push op v = "++(show value)) pushValue (stack machine) value in
                        (Machine ((pointer machine)+2) (bytecode machine) newStack (heap machine) (heapAddresses machine))

runIt :: Machine -> Machine

runIt machine 
    | (pointer machine) >= (length (bytecode machine)) = machine
    | otherwise = trace("\nIP :"++(show (pointer machine))++" \n") runIt' machine


memCode :: Int -> String
memCode mc = case mc of
    1 -> "PUSH"
    3 -> "MOV"
    4 -> "ADD"
    5 -> "SUB"
    6 -> "MUL"
    7 -> "DIV"
    _ -> "TODO"

runIt' :: Machine -> Machine        
runIt' machine 
    | (opCode machine) == 1 = trace ("\nPUSH @ "++(show machine)++"\n") runIt (pushOp machine)
    | (opCode machine) == 3 = let movIt = (movOp machine) in
            trace ("\nMOV => "++(show machine)++"\n") runIt movIt
    | (opCodeIn machine [4..7]) =             
             let opIt = (aBinaryOp machine  (opCode machine) (aBinOpFunction (opCode machine))) in
                runIt opIt
    | otherwise = Machine (-1) [] [] [] [] 


runMachine :: Machine -> Machine  
runMachine machine =  runIt machine
    
    
