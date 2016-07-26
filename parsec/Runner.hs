module Runner where 
import System.IO
import Control.Monad
import Ast
import Machine
import Stack
--import Assoc
import Debug.Trace (trace)


replaceNth n newVal (x:xs)
     | n == 0 = newVal:xs     
     | otherwise =  x:(replaceNth (n-1) newVal xs)

setInHeap :: Int -> StackValue -> [StackValue] -> [StackValue]
setInHeap address value heap = replaceNth address value heap


{- ********************************
    
    INT OPERATIONS

   ******************************** -}    

aBinOpFunction :: Int -> (Int -> Int -> Int)
aBinOpFunction op = case op of
    50 -> \x y -> x + y
    51 -> \x y -> x - y
    52 -> \x y -> x * y
    53 -> \x y -> x `quot` y



aBinaryOp :: Machine -> Int -> (Int -> Int -> Int) -> IO Machine
aBinaryOp machine opcode op = 
    let popped = (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x = (snd (fst popped)) in 
                let y =  (fst (fst popped)) in 
                    let res = ( op (getIntValue (x)) (getIntValue (y)) ) in
                        let appendedStack =  pushValue newStack (IntVal res) in
                             do return $ (Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))


aNegOp :: Machine -> IO Machine
aNegOp machine =
    let popped =  popValue (stack machine) in
        let newStack = (snd popped)  in
            let v =  (getIntValue (fst popped)) in            
                let negV = 0 - v in    
                    let appendedStack = (pushValue newStack (IntVal negV)) in                     
                         do return $ (Machine ((pointer machine ) +1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))

{- ********************************
    
    STRING OPERATIONS

   ******************************** -} 

sBinOpFunction :: Int -> (String -> String -> String)
sBinOpFunction op = case op of
    61 -> \x y ->  x ++ y

{- ********************************
    
    BOOL OPERATIONS

   ******************************** -}    

rBinOpFunction :: Int -> (Int -> Int -> Bool)
rBinOpFunction op = case op of
    58 -> \x y -> x == y
    59 -> \x y -> x > y
    60 -> \x y -> x < y
    
bBinOpFunction :: Int -> (Bool -> Bool -> Bool)
bBinOpFunction op = case op of
    56 -> \x y -> x && y
    57 -> \x y -> x ||  y
    


bBinaryOp :: Machine -> Int -> (Bool -> Bool -> Bool) -> IO Machine
bBinaryOp machine opcode op = 
    let popped = (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x =  (snd (fst popped)) in 
                let y =  (fst (fst popped)) in 
                    let res = ( op (getBoolValue (x)) (getBoolValue (y)) ) in
                        let appendedStack =  pushValue newStack (BoolVal res) in
                             do return $ (Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine)) 



bNotOp :: Machine -> IO Machine
bNotOp machine =
    let popped =  popValue (stack machine) in
        let newStack =  (snd popped)  in
            let v = (getBoolValue (fst popped)) in 
                let appendedStack =   pushValue newStack (BoolVal v) in                     
                     do return $ (Machine ((pointer machine ) +1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))



rBinaryOp :: Machine -> Int -> (Int -> Int -> Bool) -> IO Machine
rBinaryOp machine opcode op = 
    let popped =  (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x =  (snd (fst popped)) in 
                let y = (fst (fst popped)) in 
                    let res =  ( op (getIntValue (x)) (getIntValue (y)) ) in
                        let appendedStack =  pushValue newStack (BoolVal res) in
                             do return $ (Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))


sBinaryOp :: Machine -> Int -> (String -> String -> String) -> IO Machine
sBinaryOp machine opcode op = 
    let popped =  (doublePopValue (stack machine)) in
        let newStack =  (snd popped) in
            let x =  (snd (fst popped)) in 
                let y = (fst (fst popped)) in 
                    let res =  ( op (getStringValue (x)) (getStringValue (y)) ) in
                        let appendedStack =  pushValue newStack (StringVal res) in
                             do return $ (Machine ((pointer machine)+1) (bytecode machine) appendedStack (heap machine) (heapAddresses machine))                             





{- ********************************
    
    OTHER OPCODES

   ******************************** -}    

movOp :: Machine -> IO Machine
 
movOp machine = 
    let  debug =  1 in  
        let popped =  popValue (stack machine) in
            let  value =   (fst popped) in
                let bcaddr = (opCodeRel machine 1) in
                        let newheap =  setInHeap (bcaddr) value (heap machine) in
                            let newStack = (snd popped) in
                                do return $  (Machine ((pointer machine)+2) (bytecode machine) newStack newheap (heapAddresses machine))

pushOp :: Machine -> IO Machine
pushOp machine = 
    let debug =   1 in 
        let k =   ((pointer machine)+1) in
            let addr =  ((bytecode machine) !! (k)) in 
                let value =  (heap machine) !! (addr)  in
                    let newStack =  pushValue (stack machine) value in
                        do return $ (Machine ((pointer machine)+2) (bytecode machine) newStack (heap machine) (heapAddresses machine))

printOp :: Machine -> IO Machine
printOp machine =
    let popped = popValue (stack machine) in 
        let newStack = (snd popped) in
            let value = (fst popped) in
                do 
                    putStrLn (getStringValue value)
                    return $ ((Machine ((pointer machine) +1) (bytecode machine) newStack (heap machine) (heapAddresses machine)))


noOp :: Machine -> IO Machine
noOp machine = do return $ (Machine ((pointer machine) +1) (bytecode machine) (stack machine) (heap machine) (heapAddresses machine))





unconditionalJump :: Machine -> IO Machine
unconditionalJump machine = let dest =  (opCodeRel machine 1)  in 
        do  return $ (Machine dest (bytecode machine) (stack machine) (heap machine) (heapAddresses machine))

jumpIfTrue :: Machine -> IO Machine 
jumpIfTrue machine = let dest =  opCodeRel machine 1  in 
                let popped = popValue (stack machine) in
                    if (fst popped) == (BoolVal True) then                        
                        do return $ (Machine dest (bytecode machine) (stack machine) (heap machine) (heapAddresses machine))
                    else 
                        do return $ (Machine ((pointer machine)+2) (bytecode machine) (stack machine) (heap machine) (heapAddresses machine))

jumpIfNotTrue :: Machine -> IO Machine 
jumpIfNotTrue machine = let dest =   opCodeRel machine 1  in 
                let popped = popValue (stack machine) in
                    if (fst popped) == (BoolVal False) then
                        do return $ (Machine  dest (bytecode machine) (snd popped) (heap machine) (heapAddresses machine))
                    else 
                        do return $ (Machine ((pointer machine)+2)  (bytecode machine) (snd popped) (heap machine) (heapAddresses machine))                        

{- ********************************
    
    ENTRY POINT

   ******************************** -}    

runIt :: Machine -> IO Machine

runIt machine 
    | (pointer machine) >= (length (bytecode machine)) = do
        return machine
    | otherwise =  runIt' machine




genericOpCodeFunctions :: Int -> (Machine -> IO Machine)
genericOpCodeFunctions opcode = case opcode of 
    1 -> pushOp
    3 -> movOp
    54 -> aNegOp
    55 -> bNotOp
    20 -> printOp
    21 -> noOp
    10 -> unconditionalJump 
    11 -> jumpIfTrue 
    12 -> jumpIfNotTrue 
    _ -> noOp



runIt' :: Machine -> IO Machine        
runIt' machine 
    | (opCodeIn machine [stackOpsStart..otherOpEnd]) =  
        let opCodeFunction = (genericOpCodeFunctions (opCode machine)) in
            let opIt=  opCodeFunction machine  in          
                do 
                    machine <- opIt
                    runIt machine         
    | (opCodeIn machine [50..53] ) =             
             let opIt = (aBinaryOp machine  (opCode machine) (aBinOpFunction (opCode machine))) in
                do 
                    machine <- opIt
                    runIt machine       
    | (opCodeIn machine [56,57]) =             
             let opIt = (bBinaryOp machine  (opCode machine) (bBinOpFunction (opCode machine))) in
                 do 
                    machine <- opIt
                    runIt machine  
    | (opCodeIn machine [58..60]) =             
             let opIt = (rBinaryOp machine  (opCode machine) (rBinOpFunction (opCode machine))) in
                do 
                    machine <- opIt
                    runIt machine       
    | (opCodeIn machine [61]) =             
             let opIt = (sBinaryOp machine  (opCode machine) (sBinOpFunction (opCode machine))) in
                do 
                    machine <- opIt
                    runIt machine                           
    | otherwise = do
                    putStrLn ("unkown opcode "++(show (opCode machine)))
                    return $ Machine (-1) [] [] [] []


runMachine :: Machine -> IO Machine  
runMachine machine =  runIt machine
    
    
