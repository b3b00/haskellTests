module Runtime where
import System.IO
import Control.Monad
import Ast
import Debug.Trace


--play :: Stmt -> List -> IO()
play stmt map = do
    putStrLn(show(stmt))

run :: Stmt -> IO()
run stmt = do
    play stmt []

evalBExpr :: BExpr -> [a] -> (Bool,[a])
evalBExpr e map = case e of
    BoolConst b -> (b,map)
    Not expr ->  do((not(fst(evalBExpr expr map))),(snd (evalBExpr expr map)))                
    BBinary op left right -> evalBBoolOperator op left right map
    RBinary op left right -> evalRBoolOperator op left right map

evalBBoolOperator :: BBinOp -> BExpr -> BExpr -> [a] -> (Bool, [a])
evalBBoolOperator op left right map = case op of
    And  -> ((fst (evalBExpr left map)) && (fst (evalBExpr right map)), ((snd (evalBExpr left map)) ++ (snd (evalBExpr right map))))
    Or  -> ((fst (evalBExpr left map)) || (fst (evalBExpr right map)), ((snd (evalBExpr left map)) ++ (snd (evalBExpr right map))))

evalRBoolOperator :: RBinOp -> AExpr -> AExpr -> [a] -> (Bool, [a])
evalRBoolOperator op left right map = case op of
    Greater -> ((fst (evalAExpr left map)) > (fst (evalAExpr right map)), (snd (evalAExpr left map)) ++ (snd (evalAExpr right map)))    
    Less -> ((fst (evalAExpr left map)) < (fst (evalAExpr right map)), (snd (evalAExpr left map)) ++ (snd (evalAExpr right map))) 

              
evalAExpr :: AExpr -> [a] -> (Integer, [a])
evalAExpr e context = case e of
    IntConst i -> (i,context)
    Neg expr -> do
        (fst (evalAExpr expr context) , snd(evalAExpr expr context))        
        --
        --(fst res,snd res) -- negate expr        -}
    ABinary op left right -> (-42, context) -- do operation
    Var name -> (-78, context) -- lookup value in context and return it 


