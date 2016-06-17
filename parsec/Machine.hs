module Machine where 
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)
import Stack

{- 
une machine est consitutée de 
 - un bytecode [Integer]
 - une pile de valeur [StackValue]
 - un tas [StackValue] pour les constantes
-}

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
    Just n -> machine
    Nothing -> let address = length (heap machine) in
        (Machine 0 (bytecode machine) (stack machine) ((heap machine)++[NullVal]) ((heapAddresses machine)++[(name,address)]))

dump ::  [(String,Int)] -> [StackValue] -> String
dump addresses heap = case addresses of
    [] -> ""
    ((name,addr):addrs) -> (name++"::"++(show (heap !! addr))++"\n")++(dump addrs heap)

dumpHeap :: Machine -> String        
dumpHeap machine = dump (heapAddresses machine) (heap machine)