module DataStructs where

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

type Key = String
type Value = StackValue

type State = HashMap.Map Key Value

-- Create an empty stack
createEmptyStack :: Stack
createEmptyStack = []

-- Receives a stack and returns a string with the values of the stack separated by commas
stack2Str :: Stack -> String
stack2Str = intercalate "," . map stackValue2Str

-- Receives a StackValue and returns a string with the value of the StackValue
stackValue2Str :: StackValue -> String
stackValue2Str (Value x) = show x
stackValue2Str TT = "True"
stackValue2Str FF = "False"

-- Create an empty state
createEmptyState :: State
createEmptyState = HashMap.empty

-- Receives a state and returns a string with the values of the state separated by commas
state2Str :: State -> String
state2Str = intercalate "," . map pair2Str . sortOn fst . HashMap.toList

-- Receives a pair (key, value) and returns a string with the key and the value separated by an equal sign
pair2Str :: (Key, Value) -> String
pair2Str (k, v) = k ++ "=" ++ stackValue2Str v

----- Compiler data structures -----

data Aexp = Num Integer | Var String | AddAexp Aexp Aexp | MultAexp Aexp Aexp | SubAexp Aexp Aexp deriving Show

data Bexp = TruBexp | FalsBexp | NegBexp Bexp | EquBexp Aexp Aexp | LeBexp Aexp Aexp | AndBexp Bexp Bexp deriving Show

data Stm = AssignStm String Aexp | IfStm Bexp Stm Stm | WhileStm Bexp Stm | NoopStm deriving Show 