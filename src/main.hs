import DataStructs

pushN :: Either Integer Bool -> Stack -> Stack
pushN (Right True) stack = TT:stack
pushN (Right False) stack = FF:stack
pushN (Left n) stack = Value n:stack

run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined