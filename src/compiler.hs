module Compiler where

import Assembler
import DataStructs
import Data.List (isPrefixOf)

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA command =
    case command of
        Num n -> [Push n]
        Var x -> [Fetch x]
        AddAexp a1 a2 -> compA a2 ++ compA a1 ++ [Add]
        MultAexp a1 a2 -> compA a2 ++ compA a1 ++ [Mult]
        SubAexp a1 a2 -> compA a2 ++ compA a1 ++ [Sub]

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB command =
    case command of
        TruBexp -> [Tru]
        FalsBexp -> [Fals]
        NegBexp b -> compB b ++ [Neg]
        EqBexp a1 a2 -> compB a1 ++ compB a2 ++ [Equ]
        EquBexp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
        LeBexp a1 a2 -> compA a2 ++ compA a1 ++ [Le]
        AndBexp b1 b2 -> compB b1 ++ compB b2 ++ [And]

-- Compiles the program into a list of instructions
compile :: Program -> Code
compile [] = []
compile (command:rest) =
    case command of
        Aexp a -> compA a ++ compile rest
        Bexp b -> compB b ++ compile rest
        AssignStm x a -> compA a ++ [Store x] ++ compile rest
        IfStm x a b -> compB x ++ [Branch (compile a) (compile b)] ++ compile rest
        WhileStm x a -> Loop (compB x) (compile a) : compile rest

-- Auxiliary function for parse. Receives a string and splits it into a list of tokens (as a list of strings)
lexer :: String -> [String]
lexer [] = []
lexer str
    | "<=" `isPrefixOf` str = "<=" : lexer (drop 2 str)
    | "==" `isPrefixOf` str = "==" : lexer (drop 2 str)
    | ":=" `isPrefixOf` str = ":=" : lexer (drop 2 str)
    | otherwise = 
    case head str of
        ' ' -> lexer (tail str) -- We ignore spaces
        '(' -> "(" : lexer (tail str) -- Should Seperate the strings
        ')' -> ")" : lexer (tail str) -- Should Seperate the strings
        ';' -> ";" : lexer (tail str) -- Should Seperate the strings
        '=' -> "=" : lexer (tail str) -- Should Seperate the strings
        '+' -> "+" : lexer (tail str) -- Should Seperate the strings
        '-' -> "-" : lexer (tail str) -- Should Seperate the strings
        '*' -> "*" : lexer (tail str) -- Should Seperate the strings
        _ -> (head str :
            takeWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '<' && x /= ':') (tail str)) : -- While x is different of any of these, it will save them in it's own space
            lexer (dropWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '<' && x /= ':') (tail str)) -- Skips over the rest of the characters of the string that aren't these, so it doesnt parse something like ["12", "2"]

