module Stack where



data StackValue = StrVal String
           | IntVal Integer
           | BoolVal Bool
           | NullVal
            deriving (Show, Eq)

 

peekValue :: [StackValue] -> StackValue 
peekValue stack 
    | length stack == 0 = NullVal
    | length stack > 0 = stack !! (length stack -1)    

popValue :: [StackValue] -> (StackValue, [StackValue])
popValue stack 
    | length stack == 0 = (NullVal,stack)
    | length stack > 0 = (stack !! (length stack -1),init stack)

pushValue :: [StackValue] -> StackValue -> [StackValue]    
pushValue stack val = stack++[val]