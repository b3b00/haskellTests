module Stack where
import Debug.Trace (trace)


data StackValue = StrVal String
           | IntVal Int
           | BoolVal Bool
           | NullVal
            deriving (Show, Eq, Read)

 
stackValueToString sv = case sv of
    IntVal x -> "int("++(show x)++")"
    BoolVal b -> "bool("++(show b)++")"
    StrVal s -> "string("++(show s)++")"
    NullVal -> "null"

stackToString :: [StackValue] -> String
stackToString stack = case stack of
    [] -> ""
    _ -> (stackValueToString (head stack))++" ,"++(stackToString (tail stack))


peekValue :: [StackValue] -> StackValue 
peekValue stack 
    | length stack == 0 = NullVal
    | length stack > 0 = stack !! (length stack -1)    

doublePopValue :: [StackValue] -> ((StackValue,StackValue),[StackValue])

doublePopValue stack 
    | length stack < 2 = ((NullVal,NullVal),stack)
    | length stack >= 2 = let first = popValue stack in
                            let second = popValue (snd first) in
                                ((fst first,fst second),(snd second))

popValue :: [StackValue] -> (StackValue, [StackValue])
popValue stack 
    | length stack == 0 = (NullVal,stack)
    | length stack > 0 = (stack !! (length stack -1),init stack) 

pushValue :: [StackValue] -> StackValue -> [StackValue]    
pushValue stack val = stack++[val]

getBoolValue :: StackValue -> Bool
getBoolValue stackVal = case stackVal of
    IntVal i -> if i==0 then False else True
    BoolVal b -> b
    NullVal -> True

getIntValue :: StackValue -> Int
getIntValue stackVal = case stackVal of
    IntVal i -> i
    BoolVal b -> if b then 1 else 0
    NullVal -> 0    