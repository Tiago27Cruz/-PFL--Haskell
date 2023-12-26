import Assembler
import DataStructs
import Compiler



buildData :: [String] -> [Stm]
buildData [] = []

buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do
            let (bexp, rest) = break (== "then") list
                (stm1, stm2) = break (== "else") (tail rest)
            IfStm (buildBexp (tail bexp)) (buildStm (tail stm1)) (buildStm (tail stm2))
        "while" -> do
            let (bexp, stm) = break (== "do") list
            WhileStm (buildBexp (tail bexp)) (buildStm (tail stm))
        _ -> do
            let (var, aexp) = break (== "=") list
            AssignStm (head var) (buildAexp (tail aexp))



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
            if  "*" `elem` list
                then do
                    let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "*") $ reverse list
                    MultAexp (buildAexp before) (buildAexp after)
                else buildAexp (tail (init list))

buildBexp :: [String] -> Bexp
buildBexp [x] = 
    case x of
        "True" -> TruBexp
        "False" -> FalsBexp
        _ -> error "Run-time error"

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