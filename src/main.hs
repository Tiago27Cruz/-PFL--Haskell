import DataStructs
import qualified Data.Map.Strict as HashMap

-- Pushes a StackValue to the stack
push :: Either Integer Bool -> Stack -> Stack
push (Right True) stack = TT:stack
push (Right False) stack = FF:stack
push (Left n) stack = Value n:stack

-- Receives a stack, adds the two topmost values and pushes the result to the stack after removing the two topmost values
add :: Stack -> Stack
add stack =
    case stack of
        (Value n1):(Value n2):rest -> Value (n1 + n2):rest
        _ -> error "Run-time error"

-- Receives a stack, multiplies the two topmost values and pushes the result to the stack after removing the two topmost values
mult :: Stack -> Stack
mult stack =
    case stack of
        (Value n1):(Value n2):rest -> Value (n1 * n2):rest
        _ -> error "Run-time error"

-- Receives a stack, subtracts the two topmost values (in particular the first with the second) and pushes the result to the stack after removing the two topmost values
sub :: Stack -> Stack
sub stack =
    case stack of
        (Value n1):(Value n2):rest -> Value (n1 - n2):rest
        _ -> error "Run-time error"

-- Receives a stack, compares the two topmost values and pushes the result to the stack after removing the two topmost values
eq :: Stack -> Stack
eq stack =
    case stack of
        (Value n1):(Value n2):rest -> if n1 == n2 then TT:rest else FF:rest
        TT:TT:rest -> TT:rest
        FF:FF:rest -> TT:rest
        TT:FF:rest -> FF:rest
        FF:TT:rest -> FF:rest
        _ -> error "Run-time error"

-- Receives a stack, compares the two topmost values and pushes the result to the stack after removing the two topmost values. If the first value is less or equal than the second, it pushes TT, otherwise FF
le :: Stack -> Stack
le stack =
    case stack of
        (Value n1):(Value n2):rest -> if n1 <= n2 then TT:rest else FF:rest
        _ -> error "Run-time error"

-- Receives a stack, does a AND operation with the two topmost booleans and pushes the result to the stack after removing the two topmost values
ande :: Stack -> Stack
ande stack =
    case stack of
        TT:TT:rest -> TT:rest
        FF:FF:rest -> FF:rest
        TT:FF:rest -> FF:rest
        FF:TT:rest -> FF:rest
        _ -> error "Run-time error"

-- Receives a stack, negates the topmost boolean and pushes the result to the stack after removing the topmost value
neg :: Stack -> Stack
neg stack =
    case stack of
        TT:rest -> FF:rest
        FF:rest -> TT:rest
        _ -> error "Run-time error"

-- Pushes to the stack the StackValue bound to the Key received (x) in the state
fetch :: String -> Stack -> State -> Stack
fetch x stack state =
    case HashMap.lookup x state of
        Just (Value n) -> push (Left n) stack
        Just TT -> push (Right True) stack
        Just FF -> push (Right False) stack
        Nothing -> error "Run-time error"

-- Pops the topmost StackValue of the stack and binds it to the Key received (x) in the state
store :: String -> Stack -> State -> (Stack, State)
store x stack state =
    case stack of
        (Value n):rest -> (rest, HashMap.insert x (Value n) state)
        TT:rest -> (rest, HashMap.insert x TT state)
        FF:rest -> (rest, HashMap.insert x FF state)
        _ -> error "Run-time error"

-- Returns the input stack and state
noop :: Stack -> State -> (Stack, State)
noop stack state = (stack, state)

-- Pops the topmost StackValue of the stack and returns the first Code received (c1) if that value was TT, the second Code received (c2) if it was FF and fails otherwise
branch :: Code -> Code -> Stack -> (Code, Stack)
branch c1 c2 stack =
    case stack of
        TT:rest -> (c1, rest)
        FF:rest -> (c2, rest)
        _ -> error "Run-time error"

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


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)