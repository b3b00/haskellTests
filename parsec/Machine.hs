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
    bytecode :: [Int]
    , stack :: [StackValue]
    , heap :: [StackValue]
    , heapAddresses :: [(String,Int)]
} deriving (Show, Eq)
