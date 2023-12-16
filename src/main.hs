import DataStructs

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


testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run(code, createEmptyStack, createEmptyState)