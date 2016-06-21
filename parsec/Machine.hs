module Machine where 
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)
import Stack


{-
 
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
19  NOOP no operation

-}

{- 
une machine est consitutée de 
 - un bytecode [Integer]
 - une pile de valeur [StackValue]
 - un tas [StackValue] pour les constantes
-}

memCode :: Int -> String
memCode mc = case mc of
    1 -> "PUSH"
    2 -> "POP"
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
    15 -> "JMP"
    16 -> "JT"
    17 -> "JNT"
    18 -> "PRINT"
    19 -> "NOOP"    
    _ -> "UNKNOWN"

zeroParamsOpCodes = [4,5,6,7,8,9,10,11,12,13,14,18,19]


data Machine = Machine {
    pointer :: Int
    , bytecode :: [Int]
    , stack :: [StackValue]
    , heap :: [StackValue]
    , heapAddresses :: [(String,Int)]
} deriving (Show, Eq)

opCode :: Machine -> Int
opCode machine = (bytecode machine) !! (pointer machine)

opCodeRel :: Machine -> Int -> Int
opCodeRel machine offset = 
    let opcodeOffset =(pointer machine)+offset in
        let  op =    ((bytecode machine) !! opcodeOffset) in
            op 

    --(bytecode machine) !! ((pointer machine)+offset)

opCodeIn :: Machine -> [Int] -> Bool 
opCodeIn machine codes = elem (opCode machine) codes


getVariableAddress :: String -> Machine -> Int
getVariableAddress name machine = case lookup name (heapAddresses machine) of
    Just n -> n
    Nothing -> -1


-- si name E heapAddresses -> id
-- sinon heap::(name,NullVal) heapAdresses(name, len(heap))
setAddressForVariableInHeap :: String -> Machine -> Machine
setAddressForVariableInHeap name machine = case lookup name (heapAddresses machine) of 
    Just n -> trace (name ++ " already exists in machine") machine
    Nothing -> let address = length (heap machine) in
        trace (name ++ " address is now "++(show address))  (Machine 0 (bytecode machine) (stack machine) ((heap machine)++[NullVal]) ((heapAddresses machine)++[(name,address)]))

dump ::  [(String,Int)] -> [StackValue] -> String
dump addresses heap = case addresses of
    [] -> ""
    ((name,addr):addrs) -> (name++"::"++(show (heap !! addr))++"\n")++(dump addrs heap)

dumpHeap :: Machine -> String        
dumpHeap machine = dump (heapAddresses machine) (heap machine)

printAssembly :: Machine -> String
printAssembly machine  = printAssemblyBC (bytecode machine)

printAssemblyOp0 :: Int -> [Int] -> (String, [Int])  
printAssemblyOp0 op bc = ((memCode op),(drop 1 bc))

printAssemblyOp1 :: Int -> [Int] -> (String, [Int])  
printAssemblyOp1 op bc = ( ((memCode op)++" "++(show (bc!!1))) ,(drop 2 bc))


printAssemblyOp :: Int -> [Int] -> (String,[Int])
printAssemblyOp op bc 
    | (elem op zeroParamsOpCodes) = printAssemblyOp0 op bc
    | otherwise = printAssemblyOp1 op bc


printAssemblyBC :: [Int] -> String  
printAssemblyBC bc = case bc of 
    [] -> ""
    (x:xs) ->  let r = (printAssemblyOp x bc) in
                        (fst r)++"\n"++(printAssemblyBC (snd r))
              
getHeapValue:: String -> Machine -> StackValue
getHeapValue  name machine = (heap machine) !! (getVariableAddress name machine)