-- | virtual machine for parsec test
module Machine where 
import System.IO
import Control.Monad
import Ast
import Debug.Trace (trace)
import Stack


{- |
 
1.  PUSH    push a value on stack (next bc value is value address)
2.  POP pop a value from stack
3.  MOV pop value from stack and store in heap at next bytecode
4.  ADD add the two topmost values and push result
5.  SUB substract the two topmost values and push result
6.  MUL multiply the two topmost values and push result
7.  DIV divide the two topmost values and push result
8.  NEG negate top most value on stack
9.  NOT push true if top most is false, false otherwise
10.  AND push and value of 2 topmost
11.  OR  push or value of 2 topmost
12.  EQ  push true if two top values are equals
13.  GT  push true if n-1 > n
14.  LT  push true if n-1 < n
15.  JMP move code pointer to next bytecode avlue
16.  JT  move code pointer to next bytecode avlue if top stack value is True
17.  JNT move code pointer to next bytecode avlue if top stack value is False
18.  PRT print top most value
19.  NOOP no operation

-}

{- 
une machine est consitutée de 
 - un bytecode [Integer]
 - une pile de valeur [StackValue]
 - un tas [StackValue] pour les constantes
-}

{- | une machine est la structure d'une VM
 elle est constitutée de 

    * un pointeur d'instruction (l'index de l'instruction courante dans le bytecode)
    * une liste d'opcode
    * une pile de valeur utilisée à l'exécution
    * une zone mémoire (tas) pour  

        *les valeurs constantes
        *les valeurs de variables

    * la table des addresses de variables dans le tas
-}  


stackOpsStart = 1
stackOpEnd = 3

jumpOpStart = 10
jummOpEnd = 12

otherOpStart = 20
otherOpEnd = 21

binaryOpStart = 50
binaryOPEnd = 61

opcodesToMemo = [(1,"PUSH")
                ,(2,"POP")
                ,(3,"MOV")
                ,(10,"JMP")
                ,(11,"JT")
                ,(12,"JNT")
                ,(20,"PRINT")
                ,(21,"NOOP")
                ,(50,"ADD")
                ,(51,"SUB")
                ,(52,"MUL")
                ,(53,"DIV")
                ,(54,"NEG")
                ,(55,"NOT")
                ,(56,"AND")
                ,(57,"OR")
                ,(58,"EQ")
                ,(59,"GT")
                ,(60,"LT")
                ,(61,"CONCAT")
                ]

memoToOpcode :: [(String,Int)]
memoToOpcode = [(val,key) |  (key, val) <- opcodesToMemo]




data Machine = Machine {
    pointer :: Int
    , bytecode :: [Int]
    , stack :: [StackValue]
    , heap :: [StackValue]
    , heapAddresses :: [(String,Int)]
} deriving (Show, Eq, Read)


-- | retourne un memocode d'instruction (String) à partir d'un opcode
memCode :: Int -> String
memCode bc = case lookup bc opcodesToMemo of
    Just m -> m
    Nothing -> "UNKNOWN"

-- | liste des instructions (opcodes) ne prenant pas de paramètres
zeroParamsOpCodes = [stackOpsStart..stackOpEnd]++[otherOpStart..otherOpEnd]++[binaryOpStart..binaryOPEnd]

-- | retourne l'opcode de l'instruction courante de la machine
opCode :: Machine -> Int
opCode machine = (bytecode machine) !! (pointer machine)

-- retourne l'opcode situé à l'offset donné par rapport à l'instruction courante
opCodeRel :: Machine -> Int -> Int
opCodeRel machine offset = 
    let opcodeOffset =(pointer machine)+offset in
        let  op =    ((bytecode machine) !! opcodeOffset) in
            op 

    --(bytecode machine) !! ((pointer machine)+offset)

-- | retourne True si l'opcode de l'instruction courante est dans la liste en paramètre
opCodeIn :: Machine -> [Int] -> Bool 
opCodeIn machine codes = elem (opCode machine) codes


-- | retourne l'adresse dans le tas de la variable passée en paramètre
getVariableAddress :: String -> Machine -> Int
getVariableAddress name machine = case lookup name (heapAddresses machine) of
    Just n -> n
    Nothing -> -1


-- | ajoute la variable dans le tas 
-- si la  variable existe déjà ne crée pas de nouvelle entrée dans le tas
setAddressForVariableInHeap :: String -> Machine -> Machine
setAddressForVariableInHeap name machine = case lookup name (heapAddresses machine) of 
    Just n ->  machine
    Nothing -> let address = length (heap machine) in
         (Machine 0 (bytecode machine) (stack machine) ((heap machine)++[NullVal]) ((heapAddresses machine)++[(name,address)]))

-- | retourne un dump du tas pour les variables sous forme de chaine
-- à partir d'un la table d'allocation des variables et d'un tas
dump ::  [(String,Int)] -> [StackValue] -> String
dump addresses heap = case addresses of
    [] -> ""
    ((name,addr):addrs) -> (name++"::"++(show (heap !! addr))++"\n")++(dump addrs heap)

-- | retourne un dump du tas pour les variables sous forme de chaine
dumpHeap :: Machine -> String        
dumpHeap machine = dump (heapAddresses machine) (heap machine)

-- | retourne une représentation du bytecode sous forme d'un programme de memocode
printAssembly :: Machine -> String
printAssembly machine  = printAssemblyBC (bytecode machine)

printAssemblyOp0 :: Int -> [Int] -> (String, [Int])  
printAssemblyOp0 op bc = ((memCode op),(drop 1 bc))

printAssemblyOp1 :: Int -> [Int] -> (String, [Int])  
printAssemblyOp1 op bc = ( ((memCode op)++" "++(show (bc!!1))) ,(drop 2 bc))


printAssemblyOp :: Int -> [Int] -> (String,[Int])
printAssemblyOp op bc 
    | (elem op zeroParamsOpCodes) = printAssemblyOp0 op bc
    | otherwise = printAssemblyOp1 op bc


printAssemblyBC :: [Int] -> String  
printAssemblyBC bc = case bc of 
    [] -> ""
    (x:xs) ->  let r = (printAssemblyOp x bc) in
                        (fst r)++"\n"++(printAssemblyBC (snd r))

-- | retourne la valeur d'une variable               
getHeapValue:: String -> Machine -> StackValue
getHeapValue  name machine = (heap machine) !! (getVariableAddress name machine)