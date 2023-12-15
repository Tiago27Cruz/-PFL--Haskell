import DataStructs

-- Pushes a StackValue to the stack
push :: Either Integer Bool -> Stack -> Stack
push (Right True) stack = TT:stack
push (Right False) stack = FF:stack
push (Left n) stack = Value n:stack

-- Receives a stack, adds the two topmost integers and pushes the result to the stack after removing the two topmost integers
-- add :: Stack -> Stack


run :: (Code, Stack, State) -> (Code, Stack, State)
run ([], stack, state) = ([], stack, state)
run (code:rest, stack, state) = 
    case code of
        Push n -> run (rest, push (Left n) stack, state)


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)