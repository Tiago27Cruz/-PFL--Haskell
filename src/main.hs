import DataStructs

-- Pushes a StackValue to the stack
push :: Either Integer Bool -> Stack -> Stack
push (Right True) stack = TT:stack
push (Right False) stack = FF:stack
push (Left n) stack = Value n:stack

-- Receives a stack, adds the two topmost integers and pushes the result to the stack after removing the two topmost integers
add :: Stack -> Stack
add stack =
    case stack of
        (Value n1):(Value n2):rest -> Value (n1 + n2):rest
        TT:rest -> TT:add rest
        FF:rest -> FF:add rest
        _ -> error "Invalid Add Operation"


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (code:rest, stack, state) = 
    case code of
        Push n -> run (rest, push (Left n) stack, state)
        Tru -> run (rest, push (Right True) stack, state)
        Fals -> run (rest, push (Right False) stack, state)
        Add -> run (rest, add stack, state)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)