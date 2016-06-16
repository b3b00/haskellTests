module Runtime where
import System.IO
import Control.Monad
import Ast
import Assoc
import Debug.Trace (trace)


--play :: Stmt -> List -> IO()
play stmt evalContext = let    
    result = evalStmt stmt []
    in
    putStrLn (show result)

run :: Stmt -> IO()
run stmt = do
    play stmt []
    
{-
**** variable mapping
-}    
    

  
--setVariable  :: String ->     Integer -> [(String,    Integer)] -> [(String,    Integer)]

    

{-
**** statements ****
-}        

evalPrint ::     Integer -> [(String,    Integer)] -> [(String,    Integer)]
evalPrint msg evalContext =  trace ("PR    Integer :: "++show msg)  evalContext

evalSeq :: [Stmt] -> [(String,    Integer)] -> [(String,    Integer)]
evalSeq stmts evalContext = case stmts of 
    [] -> evalContext
    (stm:tail) -> evalSeq tail (evalStmt stm evalContext)

evalWhile :: BExpr -> Stmt -> [(String,    Integer)] -> [(String,    Integer)]
evalWhile cond stmt evalContext 
    | (evalBExpr cond evalContext) == False = evalContext
    | otherwise = evalWhile cond stmt (evalStmt stmt evalContext)
    
    
evalIfThenElse :: BExpr -> Stmt -> Stmt -> [(String,    Integer)] -> [(String,    Integer)]
evalIfThenElse cond stmThen stmElse evalContext
    | (evalBExpr cond evalContext)==True = (evalStmt stmThen evalContext)
    | otherwise = (evalStmt stmElse evalContext)
    
evalStmt :: Stmt -> [(String,    Integer)] -> [(String,    Integer)]
evalStmt stm evalContext = case stm of
    AssignA var expr ->  addOrReplaceInteger var (evalAExpr expr evalContext) evalContext
    --AssignB var expr ->  addOrReplaceInteger var (evalBExpr expr evalContext) evalContext
    If cond thenStmt elseStmt -> evalIfThenElse cond thenStmt elseStmt evalContext
    While cond stm -> evalWhile cond stm evalContext
    Seq stmts -> evalSeq stmts evalContext         
    Print expr -> let v = (evalAExpr expr evalContext) in        
        evalPrint (v) evalContext
    Skip -> evalContext

    
{-
**** expressions booléennes ****
-}    

evalBExpr :: BExpr -> [(String,    Integer)] -> Bool
evalBExpr e evalContext = case e of
    BoolConst b -> b
    Not expr ->  not(evalBExpr expr evalContext)
    BBinary op left right -> evalBBoolOperator op left right evalContext
    RBinary op left right -> evalRBoolOperator op left right evalContext

evalBBoolOperator :: BBinOp -> BExpr -> BExpr -> [(String,    Integer)] -> Bool
evalBBoolOperator op left right evalContext = case op of
    And  -> (evalBExpr left evalContext) && (evalBExpr right evalContext)
    Or  -> (evalBExpr left evalContext) || (evalBExpr right evalContext)

evalRBoolOperator :: RBinOp -> AExpr -> AExpr -> [(String,    Integer)] -> Bool
evalRBoolOperator op left right evalContext = case op of
    Greater -> (evalAExpr left evalContext) > (evalAExpr right evalContext)
    Less -> (evalAExpr left evalContext) < (evalAExpr right evalContext)
    Equals -> (evalAExpr left evalContext) == (evalAExpr right evalContext)

{-
**** expressions entières ****
-}    


              
evalAExpr :: AExpr -> [(String,    Integer)] ->     Integer
evalAExpr e evalContext = case e of
    IntConst i -> i
    Neg expr -> -(evalAExpr expr evalContext)
    ABinary op left right -> (evalAOperation op left right evalContext)
    Var name -> (getVariableInteger name evalContext)

evalAOperation :: ABinOp -> AExpr -> AExpr -> [(String,    Integer)] ->     Integer
evalAOperation op left right evalContext = case op of
    Add -> (evalAExpr left evalContext) + (evalAExpr right evalContext)
    Substract -> (evalAExpr left evalContext) - (evalAExpr right evalContext)
    Multiply -> (evalAExpr left evalContext) * (evalAExpr right evalContext)
    Divide -> (evalAExpr left evalContext) `quot` (evalAExpr right evalContext)
