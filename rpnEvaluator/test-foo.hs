module Main where

import RPN(splitExpression)
import System.Exit (exitFailure,exitSuccess)

testSplitExpression = (splitExpression (words " 2 2 +")) == (["2","2"],["+"])	


main = do
	case testSplitExpression of 
		False -> exitFailure
		True -> exitSuccess
    	