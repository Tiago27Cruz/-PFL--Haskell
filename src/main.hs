import DataStructs

pushN :: StackValue -> Stack -> Stack
pushN TT stack = TT:stack
pushN FF stack = FF:stack
pushN n stack = n:stack

run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined