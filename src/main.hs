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


compile :: [Stm] -> Code
compile [] = []
compile (command:rest) =
    case command of
        AssignStm x a -> compA a ++ [Store x] ++ compile rest


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