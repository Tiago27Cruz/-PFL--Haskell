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
            if head stm == "(" then buildData (tail (init stm)) -- If it's actually multiple nested statements instead of just one
            else case rest of
                [_] -> [buildStm stm] -- If it's at the last statement
                _ -> buildStm stm : buildData (tail rest) -- If it's not at the last statement, it will build the statement and call itself again with the rest of the list
        Nothing -> buildData (tail (init list)) -- If it doesn't find it, it will remove the first and last element of the list (the parentheses) and call itself again (This is only reached when it receives multiple statements that are altogether in between parentheses)

-- Builds a statement from a list of tokens that were already separated by buildData
buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do -- If it finds an "if"
            let (bexp, rest) = break (== "then") list -- It will split the list in two, before and after the "then", since before it has a boolean expression and after it has the statements
            case findNotInner ["else"] (tail rest) of -- It will look for the first "else" that's not nested
                Just index -> do -- If it finds it
                    let (stm1, stm2) = splitAt index (tail rest) -- It will split the list (rest) in two, stm1 has the statements that belong to the "then" and stm2 has the statements after the "else"
                    case head (tail stm2) of -- It will check if the first element of the statements of "else" is a "(" or not. In other words, if the "else" has multiple statements or not, if it does, they're handled by buildData, if not, buildStm can handle it. In any case, the statement of "then" are handled by buildData for ";" handling
                        "(" -> IfStm (buildBexp (tail bexp)) (buildData stm1) (buildData (tail stm2)) -- If it's a "(", it will return an IfStm with the built bolean expression obtained, the built "then" statements (or statement) obtained and the "else" statements obtained (data)
                        _ -> IfStm (buildBexp (tail bexp)) (buildData stm1) [buildStm (tail stm2)] -- If it's not a "(", it will return an IfStm with the built bolean expression obtained, the built "then" statements (or statement) obtained and the "else" statement obtained (it can be anything, even another if statement)
        "while" -> do -- If it finds a "while"
            let (bexp, stm) = break (== "do") list -- It will split the list in two, before and after the "do", since before it has a boolean expression and after it has the statements
            case head (tail stm) of -- It will check if the first element of the statements of "do" is a "(" or not. In other words, if the "do" has multiple statements or not, if it does, they're handled by buildData, if not, buildStm can handle it.
                "(" -> WhileStm (buildBexp (tail bexp)) (buildData (tail stm)) -- If it's a "(", it will return a WhileStm with the built bolean expression obtained and the built "do" statements obtained (data)
                _ -> WhileStm (buildBexp (tail bexp)) [buildStm (tail stm)] -- If it's not a "(", it will return a WhileStm with the built bolean expression obtained and the built "do" statement obtained (it can be anything, even another while statement)
        _ -> do -- If it's not an "if" or a "while"
            let (var, aexp) = break (== ":=") list -- It will split the list in two, before and after the ":=", since before it has a variable and after it has an arithmetic expression
            AssignStm (head var) (buildAexp (tail aexp)) -- It will return an AssignStm with the variable and the built arithmetic expression obtained

-- Finds the first ocurrence in a list of tokens (as a list of strings), of any token inside a given list of tokens (e.g. ["+","-"]), that's not nested and returns it's index. A token is considered nested if it's between parentheses or inside an if statement
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
            if depth == 0 && (x `elem` targets) -- If it's not nested and it finds what it's looking for
                then Just index -- It will return the index
                else find depth (index + 1) rest -- If it's not what it's looking for, it will increase the index and keep looking

-- Builds an arithmetic expression from a list of tokens
buildAexp :: [String] -> Aexp
buildAexp [x] = if all isDigit x then Num (read x) else Var x -- If it's a number, it will return a Num, otherwise it will return a Var so it accepts both
buildAexp list = 
    case findNotInner ["+","-"] (reverse list) of -- It will look for the last "+" or "-" that's not nested. It's done first because it has the least priority
        Just reversedIndex -> do -- If it finds it
            let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "+" or "-" that's not nested, since it's reversed
            let (before, after) = splitAt index list -- It will split the list in two, before and after the last "+" or "-" that's not nested
            if list!!index == "+" -- If it's a "+"
                then AddAexp (buildAexp before) (buildAexp (tail after)) -- It will return an AddAexp with the built arithmetic expressions of both lists obtained (before and after the "+")
                else SubAexp (buildAexp before) (buildAexp (tail after)) -- If it's a "-", it will return a SubAexp with the built arithmetic expressions of both lists obtained (before and after the "-")
        Nothing -> do -- If it doesn't find it
            case findNotInner ["*"] (reverse list) of -- It will look for the last "*" that's not nested, which is second in least priority
                Just reversedIndex -> do -- If it finds it
                    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "*" that's not nested, since it's reversed
                    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "*" that's not nested
                    MultAexp (buildAexp before) (buildAexp (tail after)) -- It will return a MultAexp with the built arithmetic expressions of both lists obtained (before and after the "*")
                Nothing -> buildAexp (tail (init list)) -- If it doesn't find it, then any expression left is between parentheses, so it will remove the first and last element of the list (the parentheses) and call itself again (this is done last since everything between parentheses has higher priority than what is outside of it).

-- Builds a boolean expression from a list of tokens
buildBexp :: [String] -> Bexp
buildBexp [x] = 
    case x of
        "True" -> TruBexp -- If it's "True" then it returns TruBexp
        "False" -> FalsBexp -- If it's "False" then it returns FalsBexp
        _ -> error "Run-time error" -- If it's anything else, it will throw an error
buildBexp list = 
    case findNotInner ["and"] (reverse list) of -- It will look for the last "and" that's not nested (exact match to avoid finding vars with "and" in their name). It's done first because it has the least priority 
        Just reversedIndex -> do -- If it finds it
            let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "and" that's not nested, since it's reversed
            let (before, after) = splitAt index list -- It will split the list in two, before and after the last "and" that's not nested
            AndBexp (buildBexp before) (buildBexp (tail after)) -- It will return an AndBexp with the built boolean expressions of both lists obtained (before and after the "and")
        Nothing -> do -- If it doesn't find it
            case findNotInner ["="] (reverse list) of -- It will look for the last "=" that's not nested (exact match to avoid finding "==")
                Just reversedIndex -> do -- If it finds it
                    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "=" that's not nested, since it's reversed
                    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "=" that's not nested
                    EqBexp (buildBexp before) (buildBexp (tail after)) -- It will return an EqBexp with the built boolean expressions of both lists obtained (before and after the "=")
                Nothing -> do -- If it doesn't find it
                    case findNotInner ["not"] (reverse list) of -- It will look for the last "not" that's not nested (exact match to avoid finding vars with not in their name)
                        Just reversedIndex -> do -- If it finds it 
                            let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "not" that's not nested, since it's reversed
                            let after = drop index list -- It will get what comes after the "not"
                            NegBexp (buildBexp (tail after)) -- It will return a NegBexp with the built boolean expression obtained (after the "not")
                        Nothing -> do -- If it doesn't find it
                            case findNotInner ["=="] (reverse list) of -- It will look for the last "==" that's not nested
                                Just reversedIndex -> do -- If it finds it
                                    let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "==" that's not nested, since it's reversed
                                    let (before, after) = splitAt index list -- It will split the list in two, before and after the last "==" that's not nested
                                    EquBexp (buildAexp before) (buildAexp (tail after)) -- It will return an EquBexp with the built arithmetic expressions of both lists obtained (before and after the "==")
                                Nothing -> do -- If it doesn't find it
                                    case findNotInner ["<="] (reverse list) of -- It will look for the last "<=" that's not nested (this is done last since it is the boolean operator that has the highest priority)
                                        Just reversedIndex -> do -- If it finds it
                                            let index = length list - reversedIndex - 1 -- It will calculate the real index of the last "<=" that's not nested, since it's reversed
                                            let (before, after) = splitAt index list -- It will split the list in two, before and after the last "<=" that's not nested
                                            LeBexp (buildAexp before) (buildAexp (tail after)) -- It will return a LeBexp with the built arithmetic expressions of both lists obtained (before and after the "<=")
                                        Nothing -> buildBexp (tail (init list)) -- If it doesn't find it, then any expression left is between parentheses, so it will remove the first and last element of the list (the parentheses) and call itself again (this is done last since everything between parentheses has higher priority than what is outside of it)


-- Receives a string (the program code written in the language) and returns the program
parse :: String -> Program
parse = buildData . lexer
