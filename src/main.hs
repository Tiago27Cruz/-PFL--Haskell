import Assembler
import DataStructs
import Compiler
import Data.Char (isDigit)
import Data.List (elemIndex)

buildData :: [String] -> App
buildData [] = []
buildData list = do
    let (stm, rest) = break (== ";") list
    case rest of
        [_] -> [buildStm stm] -- If at last statement
        _ -> buildStm stm : buildData (tail rest)

buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do
            let (bexp, rest) = break (== "then") list
                (stm1, stm2) = break (== "else") (tail rest)
            IfStm (buildBexp (tail bexp)) [buildStm stm1] [buildStm (tail stm2)]
        "while" -> do
            let (bexp, stm) = break (== "do") list
            case head (tail stm) of
                "(" -> WhileStm (buildBexp (tail bexp)) (buildData (drop 2 (init stm)))
                _ -> WhileStm (buildBexp (tail bexp)) [buildStm (tail stm)]
        _ -> do
            let (var, aexp) = break (== ":=") list
            AssignStm (head var) (buildAexp (tail aexp))



findNotInParens :: [String] -> [String] -> Maybe Int
findNotInParens targets list = find 0 0 list
  where
    find _ _ [] = Nothing
    find depth index (x:rest) =
        case x of
        "(" -> find (depth + 1) (index + 1) rest
        ")" -> find (depth - 1) (index + 1) rest
        _ -> do
            if depth == 0 && (x `elem` targets)
                then Just index
                else find depth (index + 1) rest

buildAexp :: [String] -> Aexp
buildAexp [x] = if all isDigit x then Num (read x) else Var x
buildAexp list = 
    case findNotInParens ["+","-"] (reverse list) of
        Just reversedIndex -> do
            let index = length list - reversedIndex - 1
            let (before, after) = splitAt index list
            if list!!index == "+"
                then AddAexp (buildAexp before) (buildAexp (tail after))
                else SubAexp (buildAexp before) (buildAexp (tail after))
        Nothing -> do
            case findNotInParens ["*"] (reverse list) of
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
    case findNotInParens ["and"] (reverse list) of
        Just reversedIndex -> do
            let index = length list - reversedIndex - 1
            let (before, after) = splitAt index list
            AndBexp (buildBexp before) (buildBexp (tail after))
        Nothing -> do
            case findNotInParens ["="] (reverse list) of
                Just reversedIndex -> do
                    let index = length list - reversedIndex - 1
                    let (before, after) = splitAt index list
                    EqBexp (buildBexp before) (buildBexp (tail after))
                Nothing -> do
                    case findNotInParens ["not"] (reverse list) of
                        Just reversedIndex -> do
                            let index = length list - reversedIndex - 1
                            let after = drop index list
                            NegBexp (buildBexp (tail after))
                        Nothing -> do
                            case findNotInParens ["=="] (reverse list) of
                                Just reversedIndex -> do
                                    let index = length list - reversedIndex - 1
                                    EquBexp (buildAexp [list!!(index-1)]) (buildAexp [list!!(index+1)])
                                Nothing -> do
                                    case findNotInParens ["<="] (reverse list) of
                                        Just reversedIndex -> do
                                            let index = length list - reversedIndex - 1
                                            LeBexp (buildAexp [list!!(index-1)]) (buildAexp [list!!(index+1)])
                                        Nothing -> buildBexp (tail (init list))


parse :: String -> [Stm]
parse = buildData . lexer


testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)