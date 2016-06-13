module Assoc where

    
{-
**** variable mapping
-}    
    
getVariable :: String -> [(String,Int)] -> Int
getVariable name evalContext = case lookup name evalContext of
  Just n  -> n
  Nothing -> 0 -- TODO : throw an error instead

addOrReplace :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplace key value assoc = (key,value):(filter ((key /=).fst) assoc)  