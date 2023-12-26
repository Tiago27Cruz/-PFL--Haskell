import Assembler
import DataStructs

compA :: Aexp -> Code
compA command =
    case command of
        Num n -> [Push n]
        Var x -> [Fetch x]
        AddAexp a1 a2 -> compA a1 ++ compA a2 ++ [Add]
        MultAexp a1 a2 -> compA a1 ++ compA a2 ++ [Mult]
        SubAexp a1 a2 -> compA a1 ++ compA a2 ++ [Sub]

compB :: Bexp -> Code
compB command =
    case command of
        TruBexp -> [Tru]
        FalsBexp -> [Fals]
        NegBexp b -> compB b ++ [Neg]
        EquBexp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
        LeBexp a1 a2 -> compA a1 ++ compA a2 ++ [Le]
        AndBexp b1 b2 -> compB b1 ++ compB b2 ++ [And]


compile :: App -> Code
compile [] = []
compile (command:rest) =
    case command of
        Aexp a -> compA a ++ compile rest
        Bexp b -> compB b ++ compile rest
        AssignStm x a -> compA a ++ [Store x] ++ compile rest
        IfStm x a b -> compB x ++ [Branch (compile a) (compile b)] ++ compile rest
        WhileStm x a -> [Loop (compB x) (compile a)] ++ compile rest

-- Auxiliary function for parse. Receives a string and splits it into a list of tokens (as a list of strings)
lexer :: String -> [String]
lexer [] = []
lexer (char:rest) =
    case char of
        ' ' -> lexer rest -- We ignore spaces
        '(' -> "(" : lexer rest -- Should Seperate Numbers
        ')' -> ")" : lexer rest -- Should Seperate Numbers
        ';' -> ";" : lexer rest -- Should Seperate Numbers
        '=' -> "=" : lexer rest -- Should Seperate Numbers
        '+' -> "+" : lexer rest -- Should Seperate Numbers
        '-' -> "-" : lexer rest -- Should Seperate Numbers
        '*' -> "*" : lexer rest -- Should Seperate Numbers
        '/' -> "/" : lexer rest -- Should Seperate Numbers
        _ -> (char :
            takeWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '/') rest) : -- While x is different of any of these, it will save them in it's own space
            lexer (dropWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '/') rest) -- Skips over the rest of the characters of the string that aren't these, so it doesnt parse something like ["12", "2"]



buildData :: [String] -> [Stm]
buildData [] = []


lastPlusOrMinus :: [String] -> Maybe String
lastPlusOrMinus strs = if null filtered then Nothing else Just (last filtered)
  where filtered = filter (\x -> x == "+" || x == "-") strs

buildAexp :: [String] -> Aexp
buildAexp [x] = Num (read x)
buildAexp list = 
    case lastPlusOrMinus list of
        Just "+" -> do
            let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "+") $ reverse list
            AddAexp (buildAexp before) (buildAexp after)
        Just "-" -> do
            let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "-") $ reverse list
            SubAexp (buildAexp before) (buildAexp after)
        Nothing -> do
            if  elem "*" list
                then do
                    let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "*") $ reverse list
                    MultAexp (buildAexp before) (buildAexp after)
                else buildAexp (tail (init list))

parse :: String -> [Stm]
parse = buildData . lexer

run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (code:rest, stack, state) = 
    case code of
        Push n -> run (rest, push (Left n) stack, state)
        Tru -> run (rest, push (Right True) stack, state)
        Fals -> run (rest, push (Right False) stack, state)
        Add -> run (rest, add stack, state)
        Mult -> run (rest, mult stack, state)
        Sub -> run (rest, sub stack, state)
        Equ -> run (rest, eq stack, state)
        Le -> run (rest, le stack, state)
        And -> run (rest, ande stack, state)
        Neg -> run (rest, neg stack, state)
        Fetch x -> run(rest, fetch x stack state, state)
        Store x -> do
            let (newStack, newState) = store x stack state
            run(rest, newStack, newState)
        Noop -> do
            let (newStack, newState) = noop stack state
            run(rest, newStack, newState)
        Branch c1 c2 -> do
            let (newRest, newStack) = branch c1 c2 stack
            run(newRest++rest, newStack, state)
        Loop c1 c2 -> run(loop c1 c2 ++ rest, stack, state)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)