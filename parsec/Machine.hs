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
opCodeRel machine offset = (bytecode machine) !! ((pointer machine)+offset)

opCodeIn :: Machine -> [Int] -> Bool 
opCodeIn machine codes = elem (opCode machine) codes