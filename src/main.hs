import qualified Data.Map.Strict as HashMap

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackValue = Value Integer | TT | FF deriving (Show, Eq)
type Stack = [StackValue]

type Key = String
type Value = StackValue

type State = HashMap.Map Key Value

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str = undefined

createEmptyState :: State
createEmptyState = undefined

state2Str :: State -> String
state2Str = undefined

run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined