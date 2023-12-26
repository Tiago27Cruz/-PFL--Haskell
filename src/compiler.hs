module Compiler where

import Assembler
import DataStructs

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
        EquBexp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
        LeBexp a1 a2 -> compA a2 ++ compA a1 ++ [Le]
        AndBexp b1 b2 -> compB b1 ++ compB b2 ++ [And]

-- Compiles the application into a list of instructions
compile :: App -> Code
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
lexer (char:rest) =
    case char of
        ' ' -> lexer rest -- We ignore spaces
        '(' -> "(" : lexer rest -- Should Seperate Numbers
        ')' -> ")" : lexer rest -- Should Seperate Numbers
        ';' -> ";" : lexer rest -- Should Seperate Numbers
        '=' -> "=" : lexer rest -- Should Seperate Numbers
        '+' -> "+" : lexer rest -- Should Seperate Numbers
        '-' -> "-" : lexer rest -- Should Seperate Numbers
        '*' -> "*" : lexer rest -- Should Seperate Numbers
        '/' -> "/" : lexer rest -- Should Seperate Numbers
        _ -> (char :
            takeWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '/') rest) : -- While x is different of any of these, it will save them in it's own space
            lexer (dropWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '/') rest) -- Skips over the rest of the characters of the string that aren't these, so it doesnt parse something like ["12", "2"]

