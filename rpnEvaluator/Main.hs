module Main where

import System.Environment
import Data.List
import Debug.Trace
import GHC.Float
import RPN

    
{-
    ******* MAIN *******
-}
main = do
  expr <- getLine
  putStrLn (show (computeRPN expr))    
    
    


      
          
