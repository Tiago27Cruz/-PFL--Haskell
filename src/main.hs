import Assembler
import DataStructs
import Compiler
import Data.Char (isDigit)
import Data.List (elemIndex)

buildData :: [String] -> App
buildData [] = []
buildData list = do
    case findNotInner [";"] list of
        Just index -> do
            let (stm, rest) = splitAt index list
            case rest of
                [_] -> [buildStm stm] -- If at last statement
                _ -> buildStm stm : buildData (tail rest)
        Nothing -> buildData (tail (init list))

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
findNotInner targets list = find 0 0 list
  where
    find _ _ [] = Nothing
    find depth index (x:rest) =
        case x of
        "(" -> find (depth + 1) (index + 1) rest
        "then" -> find (depth + 1) (index + 1) rest
        ")" -> find (depth - 1) (index + 1) rest
        "else" | depth /= 0 -> find (depth - 1) (index + 1) rest
        _ -> do
            if depth == 0 && (x `elem` targets)
                then Just index
                else find depth (index + 1) rest

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
                                    EquBexp (buildAexp [list!!(index-1)]) (buildAexp [list!!(index+1)])
                                Nothing -> do
                                    case findNotInner ["<="] (reverse list) of
                                        Just reversedIndex -> do
                                            let index = length list - reversedIndex - 1
                                            LeBexp (buildAexp [list!!(index-1)]) (buildAexp [list!!(index+1)])
                                        Nothing -> buildBexp (tail (init list))


parse :: String -> [Stm]
parse = buildData . lexer


testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)