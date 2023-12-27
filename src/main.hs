import Assembler
import DataStructs
import Compiler
import Data.Char (isDigit)
import Data.List (elemIndex)

-- Receives a list of tokens (as a list of strings) and returns a the built data program (as a list of statements)
-- 
buildData :: [String] -> Program
buildData [] = []
buildData list = do
    case findNotInner [";"] list of
        Just index -> do
            let (stm, rest) = splitAt index list
            case rest of
                [_] -> [buildStm stm] -- If at last statement
                _ -> buildStm stm : buildData (tail rest)
        Nothing -> buildData (tail (init list))

-- Builds a statement from a list of tokens that were already separated by buildData
buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do
            let (bexp, rest) = break (== "then") list
            case findNotInner ["else"] (tail rest) of
                Just index -> do
                    let (stm1, stm2) = splitAt index (tail rest)
                    case head (tail stm2) of
                        "(" -> IfStm (buildBexp (tail bexp)) (buildData stm1) (buildData (tail stm2))
                        _ -> IfStm (buildBexp (tail bexp)) (buildData stm1) [buildStm (tail stm2)]
        "while" -> do
            let (bexp, stm) = break (== "do") list
            case head (tail stm) of
                "(" -> WhileStm (buildBexp (tail bexp)) (buildData (tail stm))
                _ -> WhileStm (buildBexp (tail bexp)) [buildStm (tail stm)]
        _ -> do
            let (var, aexp) = break (== ":=") list
            AssignStm (head var) (buildAexp (tail aexp))

findNotInner :: [String] -> [String] -> Maybe Int
findNotInner targets = find 0 0
  where
    find _ _ [] = Nothing
    find depth index (x:rest) =
        case x of
        "(" -> find (depth + 1) (index + 1) rest -- If it finds a "(" it will increase the depth (as it's entering something nested) and the index
        "then" -> find (depth + 1) (index + 1) rest -- If it finds a "then" it will increase the depth and the index
        ")" -> find (depth - 1) (index + 1) rest -- If it finds a ")" it will decrease the depth (as it's leaving something that it's nested) and the index
        "else" | depth /= 0 -> find (depth - 1) (index + 1) rest -- If it finds a "else" it will decrease the depth (as it's the end of the if) and increase the index
        _ -> do
            if depth == 0 && (x `elem` targets) -- If it's not nested and it finds what's it's looking for
                then Just index -- It will return the index
                else find depth (index + 1) rest -- If it's not what it's looking for, it will increase the index and keep looking

buildAexp :: [String] -> Aexp
buildAexp [x] = if all isDigit x then Num (read x) else Var x
buildAexp list = 
    case findNotInner ["+","-"] (reverse list) of
        Just reversedIndex -> do
            let index = length list - reversedIndex - 1
            let (before, after) = splitAt index list
            if list!!index == "+"
                then AddAexp (buildAexp before) (buildAexp (tail after))
                else SubAexp (buildAexp before) (buildAexp (tail after))
        Nothing -> do
            case findNotInner ["*"] (reverse list) of
                Just reversedIndex -> do
                    let index = length list - reversedIndex - 1
                    let (before, after) = splitAt index list
                    MultAexp (buildAexp before) (buildAexp (tail after))
                Nothing -> buildAexp (tail (init list))

buildBexp :: [String] -> Bexp
buildBexp [x] = 
    case x of
        "True" -> TruBexp
        "False" -> FalsBexp
        _ -> error "Run-time error"
buildBexp list = 
    case findNotInner ["and"] (reverse list) of
        Just reversedIndex -> do
            let index = length list - reversedIndex - 1
            let (before, after) = splitAt index list
            AndBexp (buildBexp before) (buildBexp (tail after))
        Nothing -> do
            case findNotInner ["="] (reverse list) of
                Just reversedIndex -> do
                    let index = length list - reversedIndex - 1
                    let (before, after) = splitAt index list
                    EqBexp (buildBexp before) (buildBexp (tail after))
                Nothing -> do
                    case findNotInner ["not"] (reverse list) of
                        Just reversedIndex -> do
                            let index = length list - reversedIndex - 1
                            let after = drop index list
                            NegBexp (buildBexp (tail after))
                        Nothing -> do
                            case findNotInner ["=="] (reverse list) of
                                Just reversedIndex -> do
                                    let index = length list - reversedIndex - 1
                                    let (before, after) = splitAt index list
                                    EquBexp (buildAexp before) (buildAexp (tail after))
                                Nothing -> do
                                    case findNotInner ["<="] (reverse list) of
                                        Just reversedIndex -> do
                                            let index = length list - reversedIndex - 1
                                            let (before, after) = splitAt index list
                                            LeBexp (buildAexp before) (buildAexp (tail after))
                                        Nothing -> buildBexp (tail (init list))


parse :: String -> [Stm]
parse = buildData . lexer


testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)