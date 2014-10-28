module Evaluation where

import Tokenizer
import Expression
import Data.List
import Debug.Trace

data EvaluationState = EvaluationState [Macro] (Int, Int, Int, Int) deriving(Show)

isExpressionBool :: ExpressionResult -> Bool
isExpressionBool (EBool _) = True
isExpressionBool _ = False

isExpressionInt :: ExpressionResult -> Bool
isExpressionInt (EInt _) = True
isExpressionInt _ = False

isExpressionTokens :: ExpressionResult -> Bool
isExpressionTokens (ETokens _) = True
isExpressionTokens _ = False

isExpressionString :: ExpressionResult -> Bool
isExpressionString (EString _) = True
isExpressionString _ = False

isNodeSymbol :: ExpressionNode -> Bool
isNodeSymbol (ExpressionValue (TokenSymbol _)) = True
isNodeSymbol _ = False

builtinExtractValue :: [ExpressionNode] -> ExpressionResult
builtinExtractValue (ExpressionValue (TokenAddress(f)):[]) = EInt f
builtinExtractValue (ExpressionValue (TokenLiteral(f)):[]) = EInt f
builtinExtractValue (ExpressionValue (TokenIndirect(f)):[]) = EInt f
builtinExtractValue (ExpressionValue (TokenIndirectIndexed(f)):[]) = EInt f
builtinExtractValue (ExpressionValue (TokenIndexedIndirect(f)):[]) = EInt f
builtinExtractValue (ExpressionValue (TokenSymbol(f)):[]) = EString f
builtinExtractValue (ExpressionValue (TokenString(f)):[]) = EString f
builtinExtractValue (ExpressionTokenLiteral (x:[]):[]) = builtinExtractValue [ExpressionValue x]
builtinExtractValue (x:[]) = EError ("Cannot extract value of token " ++ show x)
builtinExtractValue _ = EError ("value must be 1-arity")

builtinMerge :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinMerge state = mergeChecker . map (evaluateExpression state) 
  where
    mergeChecker :: [ExpressionResult] -> ExpressionResult
    mergeChecker v
      | all isExpressionTokens v = ETokens $ mergeHelper v
      | all isExpressionString v = EString $ intercalate "" $ map (\(EString n) -> n) v
      | otherwise = EError("Cannot merge non-token values")

    mergeHelper :: [ExpressionResult] -> [Token]
    mergeHelper ((ETokens t):r) = t ++ mergeHelper r
    mergeHelper [] = []

builtinNumericFold :: (Int -> Int -> Int) -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinNumericFold fn err state = foldChecker . map (evaluateExpression state)
  where
    foldChecker :: [ExpressionResult] -> ExpressionResult
    foldChecker v
      | all isExpressionInt v = EInt $ foldl1 fn $ map (\(EInt n) -> n) v
      | otherwise = EError(err)

builtinNumericTokenizer :: (Int -> ExpressionResult) -> String -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinNumericTokenizer fn errn err state = ntokenizerChecker . map (evaluateExpression state)
  where
    ntokenizerChecker :: [ExpressionResult] -> ExpressionResult
    ntokenizerChecker v
      | length v /= 1 = EError(errn ++ " must be 1-arity")
      | all isExpressionInt v = fn ((\(EInt n) -> n) $ head v)
      | otherwise = EError(err)

builtInStringTokenizer :: (String -> ExpressionResult) -> String -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtInStringTokenizer fn errn err state = stokenizerChecker . map (evaluateExpression state)
  where
    stokenizerChecker :: [ExpressionResult] -> ExpressionResult
    stokenizerChecker v
      | length v /= 1 = EError(errn ++ " must be 1-arity")
      | all isExpressionString v = fn ((\(EString n) -> n) $ head v)
      | otherwise = EError(err)

builtinIf :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinIf state (v:a:b:[]) = ifChecker (evaluateExpression state v) [a, b]
  where
    ifChecker :: ExpressionResult -> [ExpressionNode] -> ExpressionResult
    ifChecker (EBool True) (a:b:[]) = evaluateExpression state a
    ifChecker (EBool False) (a:b:[]) = evaluateExpression state b
builtinIf state _ = EError("If must be 3-arity")

builtinEqual :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinEqual state (a:b:[]) = equalChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    equalChecker (EInt x) (EInt y) = EBool (x == y)

builtinExpand :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinExpand (EvaluationState macros (i0, i1, i2, i3)) (a:[]) = expandChecker (evaluateExpression (EvaluationState macros (i0, i1, i2, i3)) a)
  where
    expandChecker (ETokens v) = ETokens $ expandMacros (i0 + 1) i1 macros v

builtinLet :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinLet state ((ExpressionValue (TokenSymbol x)):y:z:[]) = evaluateExpression state (reifyMacroArgument z x (evaluateExpression state y))

builtinLit = builtinNumericTokenizer (\x -> ETokens [TokenLiteral x]) "lit" "Cannot create literal token from non-integer value"
builtinAddr = builtinNumericTokenizer (\x -> ETokens [TokenAddress x]) "addr" "Cannot create address token from non-integer value"
builtinInd = builtinNumericTokenizer (\x -> ETokens [TokenIndirect x]) "ind" "Cannot create indirect token from non-integer value"
builtinInIx = builtinNumericTokenizer (\x -> ETokens [TokenIndirectIndexed x]) "inix" "Cannot create indirect indexed token from non-integer value"
builtinIxIn = builtinNumericTokenizer (\x -> ETokens [TokenIndexedIndirect x]) "ixin" "Cannot create indexed indirect token from non-integer value"
builtinSym = builtInStringTokenizer(\x -> ETokens [TokenSymbol x]) "sym" "Cannot create symbol from non-string value"
builtinLabel = builtInStringTokenizer(\x -> ETokens [TokenLabel x]) "label" "Cannot create label from non-string value"

builtinAdd = builtinNumericFold (+) "Cannot add non-integer values"
builtinSub = builtinNumericFold (-) "Cannot subtract non-integer values"
builtinMul = builtinNumericFold (*) "Cannot multiply non-integer values"

evaluateExpression :: EvaluationState -> ExpressionNode -> ExpressionResult
evaluateExpression state (Expression (ExpressionValue (TokenSymbol(f)):args)) 
  | f == "value" = builtinExtractValue args
  | f == "merge" = builtinMerge state args
  | f == "+" = builtinAdd state args
  | f == "-" = builtinSub state args
  | f == "*" = builtinMul state args
  | f == "lit" = builtinLit state args
  | f == "addr" = builtinAddr state args
  | f == "ind" = builtinInd state args
  | f == "inix" = builtinInIx state args
  | f == "ixin" = builtinIxIn state args
  | f == "label" = builtinLabel state args
  | f == "sym" = builtinSym state args
  | f == "if" = builtinIf state args
  | f == "equal" = builtinEqual state args
  | f == "expand" = builtinExpand state args
  | f == "length" = lengthCheck (evaluateExpression state (args !! 0))
  | f == "let" = builtinLet state args
  | f == "id" = EString ((show i0) ++ "_" ++ (show i1) ++ "_" ++ (show i2) ++ "_" ++ (show i3))
  | otherwise = potentialMacro state f args
  where
    lengthCheck (ETokens v) = EInt (length v)
    (EvaluationState _ (i0, i1, i2, i3)) = state


evaluateExpression macros (ExpressionTokenLiteral v) = ETokens v
evaluateExpression macros (ExpressionValue (TokenAddress v)) = EInt v
evaluateExpression macros (ExpressionValue (TokenLiteral v)) = EError("Literal token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenIndirect v)) = EError("Indirect token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenIndexedIndirect v)) = EError("Indexed Indirect token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenIndirectIndexed v)) = EError("Indirect Indexed token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenString v)) = EString v
evaluateExpression macros (ExpressionRaw v) = v
evaluateExpression macros node = error ("Wtf, something went wrong: " ++ (show macros) ++ " | " ++ (show node))

reifyMacroArgument :: ExpressionNode -> String -> ExpressionResult -> ExpressionNode
reifyMacroArgument (ExpressionValue (TokenSymbol x)) argName argValue
  | x == argName = ExpressionRaw argValue
  | otherwise = (ExpressionValue (TokenSymbol x))
reifyMacroArgument (Expression v) x y = Expression (map (\n -> reifyMacroArgument n x y) v)
reifyMacroArgument n _ _ = n
 
reifyMacroArguments :: ExpressionNode -> [String] -> [ExpressionResult] -> ExpressionNode
reifyMacroArguments body args values = foldl (\x (n, v) -> reifyMacroArgument x n v) body (zip args values)

expandMacro :: EvaluationState -> [String] -> [ExpressionNode] -> ExpressionNode -> ExpressionResult
expandMacro macros argNames args body
  | length argNames /= length args = EError("Supplied arguments don't match macro arity")
  | otherwise = evaluateExpression macros $ reifyMacroArguments body argNames (map (evaluateExpression macros) args) 

potentialMacro :: EvaluationState -> String -> [ExpressionNode] -> ExpressionResult
potentialMacro (EvaluationState macros (i0, i1, i2, i3)) name argValues = potentialMacroCheck $ find (\(Macro v _ _) -> v == name) macros
  where
    potentialMacroCheck (Just (Macro _ argNames macroBody)) = expandMacro (EvaluationState macros (i0, i1, i2, i3)) argNames argValues macroBody
    potentialMacroCheck Nothing = EError("Undefined macro name: " ++ name)

readMacroDefinition :: [Token] -> (Maybe Macro, [Token])
readMacroDefinition (n:nr) = readMacroDefinition tree
  where
    (tree, remainder) = readExpression (n:nr)
    readMacroDefinition :: ExpressionNode -> (Maybe Macro, [Token])
    readMacroDefinition (Expression ((ExpressionValue (TokenSymbol "macro")) : (ExpressionValue (TokenSymbol macroName) : (Expression macroArgs) : macroBody : [])))
      | all isNodeSymbol macroArgs = (Just $ Macro macroName (map (\(ExpressionValue (TokenSymbol v)) -> v) macroArgs) macroBody, remainder)
      | otherwise = (Nothing, nr)
    readMacroDefinition _ = (Nothing, nr)

readMacroDefinitions :: [Token] -> ([Token], [Token], [Macro])
readMacroDefinitions [] = ([], [], [])
readMacroDefinitions (BeginExpression:nr) = (x, y, z)
  where
    (x, y, z) = checkForMacroDefinition $ readMacroDefinition (BeginExpression:nr)
    checkForMacroDefinition (Just x, skip) = (next, tokens, x : defs)
      where
        (next, tokens, defs) = readMacroDefinitions skip
    checkForMacroDefinition (Nothing, _) = (next, BeginExpression : tokens, defs)
      where
        (next, tokens, defs) = readMacroDefinitions nr
readMacroDefinitions (n:nr) = (x, n:y, z)
  where
    (x, y, z) = readMacroDefinitions nr

tokenify :: ExpressionResult -> [Token]
tokenify (ETokens v) = v
tokenify (EInt v) = [TokenLiteral v]
tokenify EVoid = []
tokenify (EError e) = [TokenDefer]
tokenify x = []

expandMacros :: Int -> Int -> [Macro] -> [Token] -> [Token]
expandMacros i0 i m c
  | (c /= result) = expandMacros i0 (i + 1) merged_macros result
  | otherwise = c
  where
    result = evaluateMacros 0 code
    merged_macros = m ++ macros
    (_, code, macros) = readMacroDefinitions c
    evaluateMacros i2 (BeginExpression:xr) = deferEvaluation value
      where
        deferEvaluation (EError _) = l ++ evaluateMacros (i2 + 1) r where (l, r) = readExpressionLiteral (BeginExpression:xr)
        deferEvaluation x = (tokenify x) ++ evaluateMacros (i2 + 1) remainder
        value = evaluateExpression (EvaluationState merged_macros (i0, i, i2, 0)) expression
        (expression, remainder) = readExpression (BeginExpression:xr)
    evaluateMacros i2 [] = []
    evaluateMacros i2 (x:xr) = x : evaluateMacros i2 xr