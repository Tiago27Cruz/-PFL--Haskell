import Assembler
import DataStructs
import Compiler
import Data.Char (isDigit)

buildData :: [String] -> App
buildData [] = []
buildData list = do
    let (stm, rest) = break (== ";") list
    buildStm stm : buildData (tail rest)

buildStm :: [String] -> Stm
buildStm list = 
    case head list of
        "if" -> do
            let (bexp, rest) = break (== "then") list
                (stm1, stm2) = break (== "else") (tail rest)
            IfStm (buildBexp (tail bexp)) [buildStm (tail stm1)] [buildStm (tail stm2)]
        "while" -> do
            let (bexp, stm) = break (== "do") list
            WhileStm (buildBexp (tail bexp)) [buildStm (tail stm)]
        _ -> do
            let (var, aexp) = break (== "=") list
            AssignStm (head var) (buildAexp (tail aexp))



lastPlusOrMinus :: [String] -> Maybe String
lastPlusOrMinus strs = if null filtered then Nothing else Just (last filtered)
  where filtered = filter (\x -> x == "+" || x == "-") strs

buildAexp :: [String] -> Aexp
buildAexp [x] = if all isDigit x then Num (read x) else Var x
buildAexp list = 
    case lastPlusOrMinus list of
        Just "+" -> do
            let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "+") $ reverse list
            AddAexp (buildAexp before) (buildAexp after)
        Just "-" -> do
            let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "-") $ reverse list
            SubAexp (buildAexp before) (buildAexp after)
        Nothing -> do
            if  "*" `elem` list
                then do
                    let (before, after) = (reverse (drop 1 y), reverse x) where (x, y) = break (== "*") $ reverse list
                    MultAexp (buildAexp before) (buildAexp after)
                else buildAexp (tail (init list))

buildBexp :: [String] -> Bexp
buildBexp [x] = 
    case x of
        "True" -> TruBexp
        "False" -> FalsBexp
        _ -> error "Run-time error"


parse :: String -> [Stm]
parse = buildData . lexer


testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str store)
  where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyState)