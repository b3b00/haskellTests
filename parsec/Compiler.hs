module Compiler where 
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)

{-

todo : define bytecode / assembly

-}

compileAst :: Stmt -> [Integer]
compileAst stmt = [0]
