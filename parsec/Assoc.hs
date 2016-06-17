module Assoc where

    
{-
**** variable mapping
-}    
    
getVariableInteger :: String -> [(String,Integer)] -> Integer
getVariableInteger name evalContext = case lookup name evalContext of
  Just n  -> n
  Nothing -> 0 -- TODO : throw an error instead

addOrReplaceInteger :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplaceInteger key value assoc = (key,value):(filter ((key /=).fst) assoc)  


getVariableInt :: String -> [(String,Int)] -> Int
getVariableInt name evalContext = case lookup name evalContext of
  Just n  -> n
  Nothing -> 0 -- TODO : throw an error instead

addOrReplaceInt :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
addOrReplaceInt key value assoc = (key,value):(filter ((key /=).fst) assoc)  

