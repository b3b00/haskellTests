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

1   PUSH    push a value on stack (next bc value is value address in heap)
2   POP  pop a value from stack
3   MOV     pop value from stack and store in heap at next bytecode address
4   ADD  add the two topmost values and push result 
5   SUB  substract the two topmost values and push result
6   MUL  multiply the two topmost values and push result
7   MUL  multiply the two topmost values and push result
8   EQ  push true if two top values are equals
9   GT  push true if n-1 > n
10  LT  push true if n-1 < n
11  JMP  move code pointer to next bytecode avlue 
12  JT   move code pointer to next bytecode avlue if top stack value is True (true == 1)
13  JNT      move code pointer to next bytecode avlue if top stack value is Not True  (False == 0)
14  PRT  print top most value     

-}

replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs
     | otherwise = x:replaceNth (n-1) newVal xs

setInHeap :: Int -> StackValue -> [StackValue] -> [StackValue]
setInHeap address value heap = replaceNth address value heap


movOp :: Machine -> Int -> (Int, Machine)
 
movOp machine ip = 
    let  debug = trace ("\nin mov op ") 1 in  
        let popped = trace "mov pop" popValue (stack machine) in
            let  value = trace ("mov v"++(show (fst popped))) (fst popped) in
                let addr = trace ("mov a val::"++(show value)) (bytecode machine) !! (ip+1) in
                    let newheap = trace ("mov h "++(show value)++" -> @"++(show addr)) setInHeap (addr-1) value (heap machine) in
                        let newStack = (snd popped) in
                            (ip+2,Machine (bytecode machine) newStack newheap (heapAddresses machine))



pushOp :: Machine -> Int -> (Int,Machine)
pushOp machine ip = 
    let debug = trace ("\nin push "++(show machine)) 1 in 
        let k = trace ("push op ip+1="++(show (ip+1))) (ip+1) in
            let addr = trace ("push op get head address... heap["++(show k)++"-1]") ((bytecode machine) !! (k-1)) in -- trace ("push op get head address... heap["++(show (k-1))++"] ")  in                        
                let value = trace ("push op v from head@"++(show (addr-1))) (heap machine) !! (addr-1)  in
                    let newStack = trace ("push op v = "++(show value)) pushValue (stack machine) value in
                        trace ("push -> stack="++(show newStack))(ip+2,(Machine (bytecode machine) newStack (heap machine) (heapAddresses machine)))

runIt :: (Int, Machine) -> (Int, Machine)
runIt machineState 
    | (fst machineState) >= (length (bytecode (snd machineState))) = machineState
    | otherwise = case ((bytecode (snd machineState)) !! (fst machineState)) of 
        1 -> trace ("\nPUSH @ "++(show (fst machineState))++" "++(show (snd machineState))++"\n") runIt (pushOp (snd machineState) (fst machineState))
        3 -> let movIt = (movOp (snd machineState) (fst machineState)) in
            trace ("\nMOV => "++(show (fst movIt))++" "++(show (snd movIt))++"\n") runIt movIt
        --trace ("\nmov @ "++(show (fst machineState))++" "++(show (snd machineState))++"\n")  runIt (movOp (snd machineState) (fst machineState))
        --3 -> trace ("\nmov @ "++(show (fst machineState))++" "++(show (snd machineState))++"\n") ((fst machineState)+2,(snd machineState))   
        _ -> trace ("\nDONT KNOWN ["++(show ((bytecode (snd machineState)) !! (fst machineState)))++"] @"++(show (fst machineState))++" "++(show (snd machineState))) (-1,(Machine [] [] [] [] ))

runMachine :: Int ->  Machine -> Machine  
runMachine ip machine =  snd $ runIt (ip,machine)
    
    
