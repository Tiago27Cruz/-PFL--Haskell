import qualified Data.Map.Strict as HashMap
import Data.List (intercalate, sortOn)

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data StackValue = Value Integer | TT | FF deriving (Show, Eq)
type Stack = [StackValue]

stackExample :: Stack
stackExample = [Value 5, Value 3, TT, FF, Value 2]

type Key = String
type Value = StackValue

type State = HashMap.Map Key Value

stateExample :: State
stateExample = HashMap.fromList [("a", Value 3), ("someVar", FF), ("var", TT)]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str = intercalate "," . map stack2Str_aux . reverse

stack2Str_aux :: StackValue -> String
stack2Str_aux (Value x) = show x
stack2Str_aux TT = "True"
stack2Str_aux FF = "False"

createEmptyState :: State
createEmptyState = HashMap.empty

state2Str :: State -> String
state2Str = intercalate "," . map showPair . sortOn fst . HashMap.toList

showPair :: (Key, Value) -> String
showPair (k, v) = k ++ "=" ++ stack2Str_aux v

run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined