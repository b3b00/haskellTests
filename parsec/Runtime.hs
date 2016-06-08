module Runtime where
import System.IO
import Control.Monad
import Ast
import Debug.Trace


--play :: Stmt -> List -> IO()
play stmt evalContext = do
    putStrLn(show(stmt))

run :: Stmt -> IO()
run stmt = do
    play stmt []

{-
**** statements ****
-}        

evalStmt :: Stmt -> [(String,Integer)] -> [(String,Integer)]
evalStmt stm evalContext = case stm of
    Assign var expr ->  (var, (fst (evalAExpr expr evalContext))):evalContext
    {-Print expr -> do
        v <- show (fst (evalAExpr expr evalContext))
        putStrLn v
        evalContext -}

    
{-
**** expressions booléennes ****
-}    

evalBExpr :: BExpr -> [(String,Integer)] -> (Bool,[(String,Integer)])
evalBExpr e evalContext = case e of
    BoolConst b -> (b,evalContext)
    Not expr ->  do((not(fst(evalBExpr expr evalContext))),(snd (evalBExpr expr evalContext)))                
    BBinary op left right -> evalBBoolOperator op left right evalContext
    RBinary op left right -> evalRBoolOperator op left right evalContext

evalBBoolOperator :: BBinOp -> BExpr -> BExpr -> [(String,Integer)] -> (Bool, [(String,Integer)])
evalBBoolOperator op left right evalContext = case op of
    And  -> ((fst (evalBExpr left evalContext)) && (fst (evalBExpr right evalContext)), ((snd (evalBExpr left evalContext)) ++ (snd (evalBExpr right evalContext))))
    Or  -> ((fst (evalBExpr left evalContext)) || (fst (evalBExpr right evalContext)), ((snd (evalBExpr left evalContext)) ++ (snd (evalBExpr right evalContext))))

evalRBoolOperator :: RBinOp -> AExpr -> AExpr -> [(String,Integer)] -> (Bool, [(String,Integer)])
evalRBoolOperator op left right evalContext = case op of
    Greater -> ((fst (evalAExpr left evalContext)) > (fst (evalAExpr right evalContext)), (snd (evalAExpr left evalContext)) ++ (snd (evalAExpr right evalContext)))    
    Less -> ((fst (evalAExpr left evalContext)) < (fst (evalAExpr right evalContext)), (snd (evalAExpr left evalContext)) ++ (snd (evalAExpr right evalContext))) 

{-
**** expressions entières ****
-}    

getVariable :: String -> [(String,Integer)] -> Integer
getVariable name evalContext = case lookup name evalContext of
  Just n  -> n
  Nothing -> 0 -- TODO : throw an error instead
              
evalAExpr :: AExpr -> [(String,Integer)] -> (Integer, [(String,Integer)])
evalAExpr e evalContext = case e of
    IntConst i -> (i,evalContext)
    Neg expr -> do
        ((-(fst (evalAExpr expr evalContext))) , evalContext)                
    ABinary op left right -> ((evalAOperation op left right evalContext),evalContext) 
    Var name -> ((getVariable name evalContext), evalContext) -- lookup value in evalContext and return it 

evalAOperation :: ABinOp -> AExpr -> AExpr -> [(String,Integer)] -> Integer
evalAOperation op left right evalContext = case op of
    Add -> ((fst (evalAExpr left evalContext)) + (fst (evalAExpr right evalContext)))
    Substract -> ((fst (evalAExpr left evalContext)) - (fst (evalAExpr right evalContext)))
    Multiply -> ((fst (evalAExpr left evalContext)) * (fst (evalAExpr right evalContext)))
    Divide -> ((fst (evalAExpr left evalContext)) `quot` (fst (evalAExpr right evalContext)))
