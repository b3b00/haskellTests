import System.Environment
import Data.List
import Debug.Trace

myProduct :: (Num a) => [a] -> a
myProduct = foldr1 (*)    

fact2 :: (Integral a) => a -> a
fact2 n = myProduct [1..n] 



isNumber :: String -> Bool
isNumber str =
    case (reads str) :: [(Double, String)] of
      [(_, "")] -> True
      _         -> False


prse ::  [String] -> [String]
prse = takeWhile isNumber

isOperation :: String -> Bool
isOperation str = str == "+" || str == "-" || str == "*" || str == "/"

splitExpression :: [String] -> ([String],[String])
splitExpression expr = ((filter isNumber expr), (filter isOperation expr))

convert :: ([String],[String]) -> ([Float],[String]) 
convert (n,o) = ((map (read::String->Float) n),o) 

parse :: String -> ([Float],[String])
parse code = convert (splitExpression (words code))

eval :: [Float] -> [String] -> Float
eval nums ops
	| null ops = head nums
	| (length ops >= 1) && (length nums >= 2) = eval ([(evaluate (head ops) (take 2 nums))]++(drop 2 nums)) (tail ops)
	| otherwise = error "bad expression, unaleb to evaluate"

rpn :: String -> Float
rpn expr = eval n o
	where (n,o) = parse expr

{- operation String -> Float -> Float -> Float
evaluate op args
   | op == "+" = (+)
   | op == "-" = (-)
   | op == "*" = (*)
   | op == "/" = (/)
   | otherwise = error ("unknown operation : " ++ op) -}
   
    
	
evaluate :: String -> [Float] -> Float	
evaluate op args
   | op == "+" = traceShow ("+ "++(show args)) args!!0 + args!!1
   | op == "-" = traceShow ("- "++(show args))args!!0 - args!!1
   | op == "*" = traceShow ("* "++(show args))args!!0 * args!!1
   | op == "/" = traceShow ("/ "++(show args))args!!0 / args!!1
   | otherwise = error ("unknown operation : " ++ op)

	

-- | otherwise = error ("bad oper " ++ op)
	
  
solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs  	
   
main = do
	expr <- getLine
	putStrLn (show (rpn expr))	
	


	  
		  
