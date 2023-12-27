module Compiler where

import Assembler
import DataStructs
import Data.Char (isDigit)
import Data.List (elemIndex, isPrefixOf)

-- Compiles an arithmetic expression into a list of instructions
compA :: Aexp -> Code
compA command =
    case command of
        Num n -> [Push n]
        Var x -> [Fetch x]
        AddAexp a1 a2 -> compA a2 ++ compA a1 ++ [Add] -- a2 comes before a1 because the stack is LIFO
        MultAexp a1 a2 -> compA a2 ++ compA a1 ++ [Mult] -- a2 comes before a1 because the stack is LIFO
        SubAexp a1 a2 -> compA a2 ++ compA a1 ++ [Sub] -- a2 comes before a1 because the stack is LIFO

-- Compiles a boolean expression into a list of instructions
compB :: Bexp -> Code
compB command =
    case command of
        TruBexp -> [Tru]
        FalsBexp -> [Fals]
        NegBexp b -> compB b ++ [Neg]
        EqBexp a1 a2 -> compB a1 ++ compB a2 ++ [Equ]
        EquBexp a1 a2 -> compA a1 ++ compA a2 ++ [Equ]
        LeBexp a1 a2 -> compA a2 ++ compA a1 ++ [Le] -- a2 comes before a1 because the stack is LIFO
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
    | "<=" `isPrefixOf` str = "<=" : lexer (drop 2 str) -- If the string starts with "<=", it will add it to the list of tokens and call lexer again with the rest of the string
    | "==" `isPrefixOf` str = "==" : lexer (drop 2 str) -- If the string starts with "==", it will add it to the list of tokens and call lexer again with the rest of the string
    | ":=" `isPrefixOf` str = ":=" : lexer (drop 2 str) -- If the string starts with ":=", it will add it to the list of tokens and call lexer again with the rest of the string
    | otherwise = 
    case head str of
        ' ' -> lexer (tail str) -- We ignore spaces and call lexer again with the rest of the string
        '(' -> "(" : lexer (tail str) -- If the string starts with "(", it will add it to the list of tokens and call lexer again with the rest of the string
        ')' -> ")" : lexer (tail str) -- If the string starts with ")", it will add it to the list of tokens and call lexer again with the rest of the string
        ';' -> ";" : lexer (tail str) -- If the string starts with ";", it will add it to the list of tokens and call lexer again with the rest of the string
        '=' -> "=" : lexer (tail str) -- If the string starts with "=", it will add it to the list of tokens and call lexer again with the rest of the string
        '+' -> "+" : lexer (tail str) -- If the string starts with "+", it will add it to the list of tokens and call lexer again with the rest of the string
        '-' -> "-" : lexer (tail str) -- If the string starts with "-", it will add it to the list of tokens and call lexer again with the rest of the string
        '*' -> "*" : lexer (tail str) -- If the string starts with "*", it will add it to the list of tokens and call lexer again with the rest of the string
        _ -> (head str :
            takeWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '<' && x /= ':') (tail str)) : -- While x is different of any of these, it will save them in it's own space
            lexer (dropWhile (\x -> x /= ' ' && x /= '(' && x /= ')' && x /= ';' && x /= '=' && x /= '+' && x /= '-' && x /= '*' && x /= '<' && x /= ':') (tail str)) -- Skips over the rest of the characters of the string that aren't these, so it doesnt parse something like ["12", "2"]

-- Receives a list of tokens (as a list of strings) and returns the built data program (as a list of statements)
buildData :: [String] -> Program
buildData [] = []
buildData list = do
    case findNotInner [";"] list of -- It will look for the first ";" that's not nested or that belongs to an if statement
        Just index -> do -- If it finds it
            let (stm, rest) = splitAt index list -- It will split the list in two, before and after the first ";" that's not nested
            case rest of
                [_] -> [buildStm stm] -- If it's at the last statement
                _ -> buildStm stm : buildData (tail rest) -- If it's not at the last statement, it will build the statement and call itself again with the rest of the list
        Nothing -> buildData (tail (init list)) -- If it doesn't find it, it will remove the first and last element of the list (the parenthesis) and call itself again (this is done last since everything under parenthesis has the higher priority than what is outside of it).

-- Builds a statement from a list of tokens that were already separated by buildData
buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do -- If it finds an "if"
            let (bexp, rest) = break (== "then") list -- It will split the list in two, before and after the "then", since before it has a boolean expression and after it has the statements
            case findNotInner ["else"] (tail rest) of -- It will look for the first "else" that's not nested
                Just index -> do -- If it finds it
                    let (stm1, stm2) = splitAt index (tail rest) -- It will split the list (rest) in two, stm1 that has the statements that belong to the "then" and stm2 that has the statements after the "else"
                    case head (tail stm2) of -- It will check if the first element of the rest of the "else" is a "(" or not
                        "(" -> IfStm (buildBexp (tail bexp)) (buildData stm1) (buildData (tail stm2)) -- If it's a "(", it will return an IfStm using the list it splitted before (before and after the "then" and before and after the "else") and build the data both have (so it can be anything, even another if statement)
                        _ -> IfStm (buildBexp (tail bexp)) (buildData stm1) [buildStm (tail stm2)] -- If it's not a "(", it will return an IfStm using the list it splitted before (before and after the "then" and before and after the "else") and build the data of the "then" and the statement of the "else" (inside [] since it needs to be a list)
        "while" -> do -- If it finds a "while"
            let (bexp, stm) = break (== "do") list -- It will split the list in two, before and after the "do", since before it has a boolean expression and after it has the statements 
            case head (tail stm) of -- It will check if the first element of the rest of the "do" is a "(" or not
                "(" -> WhileStm (buildBexp (tail bexp)) (buildData (tail stm)) -- If it's a "(", it will return a WhileStm using the list it splitted before (before and after the "do") and build the data both have (so it can be anything, even another if statement)
                _ -> WhileStm (buildBexp (tail bexp)) [buildStm (tail stm)] -- If it's not a "(", it will return a WhileStm using the list it splitted before (before and after the "do") and build the statement of the "do" (inside [] since it needs to be a list)
        _ -> do -- If it's not an "if" or a "while"
            let (var, aexp) = break (== ":=") list -- It will split the list in two, before and after the ":=", since before it has a variable and after it has an arithmetic expression
            AssignStm (head var) (buildAexp (tail aexp)) -- It will return an AssignStm using the list it splitted before (before and after the ":=") and build the arithmetic expression

-- Finds the first ocurrence of any token inside a given list of tokens (like ["+","-"]) that's not nested in a list of tokens (as a list of strings) and returns it's index
findNotInner :: [String] -> [String] -> Maybe Int
findNotInner targets = find 0 0 -- It will call the find function with depth 0, index 0 and the list of tokens
  where
    find _ _ [] = Nothing -- If it reaches the end of the list or given an empty list, it will return Nothing
    find depth index (x:rest) =
        case x of
        "(" -> find (depth + 1) (index + 1) rest -- If it finds a "(" it will increase the depth (as it's entering something nested) and the index
        "then" -> find (depth + 1) (index + 1) rest -- If it finds a "then" it will increase the depth and the index (since it's inside the if statement)
        ")" -> find (depth - 1) (index + 1) rest -- If it finds a ")" it will decrease the depth (as it's leaving something that it's nested) and the index
        "else" | depth /= 0 -> find (depth - 1) (index + 1) rest -- If it finds a "else" it will decrease the depth (as it's the end of the if) and increase the index
        _ -> do
            if depth == 0 && (x `elem` targets) -- If it's not nested and it finds what's it's looking for
                then Just index -- It will return the index
                else find depth (index + 1) rest -- If it's not what it's looking for, it will increase the index and keep looking

-- Builds an arithmetic expression from a list of tokens
buildAexp :: [String] -> Aexp
buildAexp [x] = if all isDigit x then Num (read x) else Var x -- If it's a number, it will return a Num, otherwise it will return a Var so it accepts both
buildAexp list = 
    case findNotInner ["+","-"] (reverse list) of -- It will look for the first "+" or "-" that's not nested. It's done first because it has the least priority
        Just reversedIndex -> do -- If it finds it
            let index = length list - reversedIndex - 1 -- It will calculate the index of the first "+" or "-" that's not nested
            let (before, after) = splitAt index list -- It will split the list in two, before and after the first "+" or "-" that's not nested
            if list!!index == "+" -- If it's a "+"
                then AddAexp (buildAexp before) (buildAexp (tail after)) -- It will return an AddAexp using the list it splitted before (before and after the "+") and build the arithmetic expressions of both
                else SubAexp (buildAexp before) (buildAexp (tail after)) -- If it's a "-", it will return a SubAexp using the list it splitted before (before and after the "-") and build the arithmetic expressions of both
        Nothing -> do -- If it doesn't find it
            case findNotInner ["*"] (reverse list) of -- It will look for the first "*" that's not nested
                Just reversedIndex -> do -- If it finds it
                    let index = length list - reversedIndex - 1 -- It will calculate the index of the first "*" that's not nested
                    let (before, after) = splitAt index list -- It will split the list in two, before and after the first "*" that's not nested
                    MultAexp (buildAexp before) (buildAexp (tail after)) -- It will return a MultAexp using the list it splitted before (before and after the "*") and build the arithmetic expressions of both
                Nothing -> buildAexp (tail (init list)) -- If it doesn't find it, it will remove the first and last element of the list (the parenthesis) and call itself again (this is done last since everything under parenthesis has the higher priority than what is outside of it).

-- Builds a boolean expression from a list of tokens
buildBexp :: [String] -> Bexp
buildBexp [x] = 
    case x of
        "True" -> TruBexp -- If it's "True" then it returns TruBexp
        "False" -> FalsBexp -- If it's "False" then it returns FalsBexp
        _ -> error "Run-time error" -- If it's anything else, it will throw an error
buildBexp list = 
    case findNotInner ["and"] (reverse list) of -- It will look for the first "and" that's not nested (exact match to avoid finding vars with and in their name). It's done first because it has the least priority 
        Just reversedIndex -> do -- If it finds it
            let index = length list - reversedIndex - 1 -- It will calculate the index of the first "and" that's not nested
            let (before, after) = splitAt index list -- It will split the list in two, before and after the first "and" that's not nested
            AndBexp (buildBexp before) (buildBexp (tail after)) -- It will return an AndBexp using the list it splitted before (before and after the "and") and build the boolean expressions of both
        Nothing -> do -- If it doesn't find it
            case findNotInner ["="] (reverse list) of -- It will look for the first "=" that's not nested (exact match to avoid finding "==")
                Just reversedIndex -> do -- If it finds it
                    let index = length list - reversedIndex - 1 -- It will calculate the index of the first "=" that's not nested
                    let (before, after) = splitAt index list -- It will split the list in two, before and after the first "=" that's not nested
                    EqBexp (buildBexp before) (buildBexp (tail after)) -- It will return an EqBexp using the list it splitted before (before and after the "=") and build the boolean expressions of both
                Nothing -> do -- If it doesn't find it
                    case findNotInner ["not"] (reverse list) of -- It will look for the first "not" that's not nested (exact match to avoid finding vars with not in their name)
                        Just reversedIndex -> do -- If it finds it 
                            let index = length list - reversedIndex - 1 -- It will calculate the index of the first "not" that's not nested
                            let after = drop index list -- It will get what comes after the "not"
                            NegBexp (buildBexp (tail after)) -- It will return a NegBexp and build the boolean expression of what comes after the "not"
                        Nothing -> do -- If it doesn't find it
                            case findNotInner ["=="] (reverse list) of -- It will look for the first "==" that's not nested
                                Just reversedIndex -> do -- If it finds it
                                    let index = length list - reversedIndex - 1 -- It will calculate the index of the first "==" that's not nested
                                    let (before, after) = splitAt index list -- It will split the list in two, before and after the first "==" that's not nested
                                    EquBexp (buildAexp before) (buildAexp (tail after)) -- It will return an EquBexp using the list it splitted before (before and after the "==") and build the arithmetic expressions of both
                                Nothing -> do -- If it doesn't find it
                                    case findNotInner ["<="] (reverse list) of -- It will look for the first "<=" that's not nested (this is done last since it is the boolean operator that has the highest priority)
                                        Just reversedIndex -> do -- If it finds it
                                            let index = length list - reversedIndex - 1 -- It will calculate the index of the first "<=" that's not nested
                                            let (before, after) = splitAt index list -- It will split the list in two, before and after the first "<=" that's not nested
                                            LeBexp (buildAexp before) (buildAexp (tail after)) -- It will return a LeBexp using the list it splitted before (before and after the "<=") and build the arithmetic expressions of both
                                        Nothing -> buildBexp (tail (init list)) -- If it doesn't find it, it will remove the first and last element of the list (the parenthesis) and call itself again (this is done last since everything under parenthesis has the higher priority than what is outside of it)


-- Receives a string (the program code written in the language) and returns the program
parse :: String -> Program
parse = buildData . lexer
