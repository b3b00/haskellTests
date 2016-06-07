module Runtime where
import System.IO
import Control.Monad
import Ast

run :: Stmt -> IO()
run stmt = do
    putStrLn (show(stmt))
    