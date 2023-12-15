import qualified Data.Map.Strict as HashMap
import Data.List (intercalate, sortOn)

-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- The values of something inside a stack are either an Integer or a boolean.
data StackValue = Value Integer | TT | FF deriving (Show, Eq)
type Stack = [StackValue]

stackExample :: Stack
stackExample = [Value 5, Value 3, TT, FF, Value 2]

type Key = String
type Value = StackValue

type State = HashMap.Map Key Value

stateExample :: State
stateExample = HashMap.fromList [("a", Value 3), ("someVar", FF), ("var", TT)]

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Receives a stack and returns a string with the values of the stack separated by commas
stack2Str :: Stack -> String
stack2Str = intercalate "," . map showStackValue . reverse

-- Receives a StackValue and returns a string with the value of the StackValue
showStackValue :: StackValue -> String
showStackValue (Value x) = show x
showStackValue TT = "True"
showStackValue FF = "False"

-- Create an empty state
createEmptyState :: State
createEmptyState = HashMap.empty

-- Receives a state and returns a string with the values of the state separated by commas
state2Str :: State -> String
state2Str = intercalate "," . map showPair . sortOn fst . HashMap.toList

-- Receives a pair (key, value) and returns a string with the key and the value separated by an equal sign
showPair :: (Key, Value) -> String
showPair (k, v) = k ++ "=" ++ showStackValue v

run :: (Code, Stack, State) -> (Code, Stack, State)
run = undefined