module Evaluation where

import PTokenizer
import Expression
import Data.List
import Debug.Trace
import qualified Data.Map as Map

data EvaluationState = EvaluationState (Map.Map String Macro) (Int, Int, Int, Int) deriving(Show)

newEvaluationState :: EvaluationState
newEvaluationState = EvaluationState (Map.empty) (0, 0, 0, 0)

firstError :: [ExpressionResult] -> Maybe ExpressionResult
firstError (EError e:y) = Just $ EError e
firstError [] = Nothing
firstError (x:xr) = firstError xr

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

conditionalError :: [ExpressionResult] -> String -> ExpressionResult
conditionalError results errorString = error
  where
    error = errorCheck $ firstError results
    errorCheck Nothing = EError errorString
    errorCheck (Just x) = x

builtinMerge :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinMerge state = mergeChecker . map (evaluateExpression state) 
  where
    mergeChecker :: [ExpressionResult] -> ExpressionResult
    mergeChecker v
      | all isExpressionTokens v = ETokens $ mergeHelper v
      | all isExpressionString v = EString $ intercalate "" $ map (\(EString n) -> n) v
      | otherwise = conditionalError v ("Cannot merge non-token values: " ++ show v)
    mergeHelper :: [ExpressionResult] -> [Token]
    mergeHelper ((ETokens t):r) = t ++ mergeHelper r
    mergeHelper [] = []

builtinNumericFold :: (Int -> Int -> Int) -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinNumericFold fn err state = foldChecker . map (evaluateExpression state)
  where
    foldChecker :: [ExpressionResult] -> ExpressionResult
    foldChecker v
      | all isExpressionInt v = EInt $ foldl1 fn $ map (\(EInt n) -> n) v
      | otherwise = conditionalError v err

builtinBooleanFold :: (Bool -> Bool -> Bool) -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinBooleanFold fn err state = foldChecker . map (evaluateExpression state)
  where
    foldChecker :: [ExpressionResult] -> ExpressionResult
    foldChecker v
      | all isExpressionBool v = EBool $ foldl1 fn $ map (\(EBool n) -> n) v

builtinNumericTokenizer :: (Int -> ExpressionResult) -> String -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinNumericTokenizer fn errn err state = ntokenizerChecker . map (evaluateExpression state)
  where
    ntokenizerChecker :: [ExpressionResult] -> ExpressionResult
    ntokenizerChecker v
      | length v /= 1 = EError(errn ++ " must be 1-arity")
      | all isExpressionInt v = fn ((\(EInt n) -> n) $ head v)
      | otherwise = conditionalError v err

builtinStringTokenizer :: (String -> ExpressionResult) -> String -> String -> EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinStringTokenizer fn errn err state = stokenizerChecker . map (evaluateExpression state)
  where
    stokenizerChecker :: [ExpressionResult] -> ExpressionResult
    stokenizerChecker v
      | length v /= 1 = EError(errn ++ " must be 1-arity")
      | all isExpressionString v = fn ((\(EString n) -> n) $ head v)
      | otherwise = conditionalError v err

builtinIf :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinIf state (v:a:b:[]) = ifChecker (evaluateExpression state v) [a, b]
  where
    ifChecker :: ExpressionResult -> [ExpressionNode] -> ExpressionResult
    ifChecker (EBool True) (a:b:[]) = evaluateExpression state a
    ifChecker (EBool False) (a:b:[]) = evaluateExpression state b
    ifChecker (n) (a:b:[]) = conditionalError [n] "If statement first argument must be boolean"
builtinIf state _ = EError "If must be 3-arity"

builtinEqual :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinEqual state (a:b:[]) = equalChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    equalChecker (EInt x) (EInt y) = EBool (x == y)
    equalChecker x y = conditionalError [x, y] "Both operands of equal must be integers"
builtinEqual state _ = EError "Equal must be 2-arity"

builtinGreater :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinGreater state (a:b:[]) = equalChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    equalChecker (EInt x) (EInt y) = EBool (x > y)
    equalChecker x y = conditionalError [x, y] "Both operands of greater must be integers"
builtinGreater state _ = EError "Greater must be 2-arity"

builtinLesser :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinLesser state (a:b:[]) = equalChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    equalChecker (EInt x) (EInt y) = EBool (x < y)
    equalChecker x y = conditionalError [x, y] "Both operands of lesser must be integers"
builtinLesser state _ = EError "Lesser must be 2-arity"

builtinExpand :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinExpand (EvaluationState macros (i0, i1, i2, i3)) (a:[]) = expandChecker (evaluateExpression (EvaluationState macros (i0, i1, i2, i3)) a)
  where
    expandChecker (ETokens v) = ETokens $ expandMacros (EvaluationState macros (i0, i1, i2, i3 + 1)) v
    expandChecker (v) = conditionalError [v] "Expand's first argument must be tokens"
builtinExpand _ _ = EError "expand must be 1-arity"


builtinLambda :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinLambda state ((Expression e):v:[]) = lambdaChecker e v
  where
    lambdaChecker :: [ExpressionNode] -> ExpressionNode -> ExpressionResult
    lambdaChecker args body
      | all isArgumentSymbol args = ELambda (extractArgumentStrings args) v
      | otherwise = EError "First argument of lambda must be a set of symbols"

    extractArgumentStrings :: [ExpressionNode] -> [String]
    extractArgumentStrings [] = []
    extractArgumentStrings ((ExpressionValue (TokenSymbol v)):r) = v : extractArgumentStrings r
    extractArgumentStrings x = error "Mysterious condition!"

    isArgumentSymbol :: ExpressionNode -> Bool
    isArgumentSymbol (ExpressionValue (TokenSymbol v)) = True
    isArgumentSymbol x = False

builtinLambda state (e:(Expression v):[]) = EError "First argument of lambda must be a set of symbols"
builtinLambda state (e:v:[]) = EError "Second argument of lambda must be an expression"
builtinLambda state x = EError "Lambda must be 2-arity"

builtinApply :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinApply state (fn:r) = applyChecker (evaluateExpression state fn) (map (evaluateExpression state) r)
  where
    applyChecker :: ExpressionResult -> [ExpressionResult] -> ExpressionResult
    applyChecker (ELambda largs body) args
      | length args == length largs = evaluateExpression state $ reifyMacroArguments body largs args
      | otherwise = EError $ "Arity of lambda doesn't match arity of apply: " ++ (show $ length args) ++ "/" ++ (show $ length largs)
builtinApply state x = EError "Apply must be of 1-arity or more"

builtinLet :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinLet state ((ExpressionValue (TokenSymbol x)):y:z:[]) = evaluateExpression state (reifyMacroArgument z x (evaluateExpression state y))
builtinLet state (x:y:z:[]) = EError "First argument to Let must be a symbol"
builtinLet state _ = EError "Let must be 3-arity"

builtinLit = builtinNumericTokenizer (\x -> ETokens [TokenLiteral x]) "lit" "Cannot create literal token from non-integer value"
builtinAddr = builtinNumericTokenizer (\x -> ETokens [TokenAddress x]) "addr" "Cannot create address token from non-integer value"
builtinInd = builtinNumericTokenizer (\x -> ETokens [TokenIndirect x]) "ind" "Cannot create indirect token from non-integer value"
builtinInIx = builtinNumericTokenizer (\x -> ETokens [TokenIndirectIndexed x]) "inix" "Cannot create indirect indexed token from non-integer value"
builtinIxIn = builtinNumericTokenizer (\x -> ETokens [TokenIndexedIndirect x]) "ixin" "Cannot create indexed indirect token from non-integer value"
builtinSym = builtinStringTokenizer(\x -> ETokens [TokenSymbol x]) "sym" "Cannot create symbol from non-string value"
builtinLabel = builtinStringTokenizer(\x -> ETokens [TokenLabel x]) "label" "Cannot create label from non-string value"

builtinAdd = builtinNumericFold (+) "Cannot add non-integer values"
builtinSub = builtinNumericFold (-) "Cannot subtract non-integer values"
builtinMul = builtinNumericFold (*) "Cannot multiply non-integer values"

builtinOr = builtinBooleanFold (||) "Cannot OR non-boolean values"
builtinAnd = builtinBooleanFold (&&) "Cannot AND non-boolean values"

builtinNot :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinNot state (a:[]) = notChecker (evaluateExpression state a)
  where
    notChecker (EBool n) = EBool $ not n
    notChecker x = conditionalError [x] "Argument to not must be a boolean value"
builtinNot state _ = EError "Not must be 1-arity"

evaluateExpression :: EvaluationState -> ExpressionNode -> ExpressionResult
evaluateExpression state (Expression (ExpressionValue (TokenSymbol(f)):args)) 
  | f == "merge" = builtinMerge state args
  | f == "&" = builtinMerge state args
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
  | f == "greater" = builtinGreater state args
  | f == "lesser" = builtinLesser state args
  | f == "or" = builtinOr state args
  | f == "and" = builtinAnd state args
  | f == "not" = builtinNot state args
  | f == "expand" = builtinExpand state args
  | f == "let" = builtinLet state args
  | f == "id" = EString ((show i0) ++ "_" ++ (show i1) ++ "_" ++ (show i2) ++ "_" ++ (show i3))
  | f == "lambda" = builtinLambda state args
  | f == "%" = builtinLambda state args
  | f == "apply" = builtinApply state args
  | otherwise = potentialMacro state f args
  where
    (EvaluationState _ (i0, i1, i2, i3)) = state


evaluateExpression state (ExpressionTokenLiteral v) = ETokens v
evaluateExpression state (ExpressionValue (TokenAddress v)) = EInt v
evaluateExpression state (ExpressionValue (TokenLiteral v)) = EError("Literal token is not a valid expression type")
evaluateExpression state (ExpressionValue (TokenIndirect v)) = EError("Indirect token is not a valid expression type")
evaluateExpression state (ExpressionValue (TokenIndexedIndirect v)) = EError("Indexed Indirect token is not a valid expression type")
evaluateExpression state (ExpressionValue (TokenIndirectIndexed v)) = EError("Indirect Indexed token is not a valid expression type")
evaluateExpression state (ExpressionValue (TokenString v)) = EString v
evaluateExpression state (Expression ((ExpressionRaw (ELambda args body)):r)) = builtinApply state ((ExpressionRaw (ELambda args body)):r)
evaluateExpression state (ExpressionRaw v) = v
evaluateExpression state node = error ("Wtf, something went wrong: " ++ (show state) ++ " | " ++ (show node))

reifyMacroArgument :: ExpressionNode -> String -> ExpressionResult -> ExpressionNode
reifyMacroArgument (ExpressionValue (TokenSymbol x)) argName argValue
  | x == argName = ExpressionRaw argValue
  | otherwise = (ExpressionValue (TokenSymbol x))
reifyMacroArgument (Expression v) x y = Expression (map (\n -> reifyMacroArgument n x y) v)
reifyMacroArgument n _ _ = n
 
reifyMacroArguments :: ExpressionNode -> [String] -> [ExpressionResult] -> ExpressionNode
reifyMacroArguments body args values = foldl (\x (n, v) -> reifyMacroArgument x n v) body (zip args values)

expandMacro :: EvaluationState -> [String] -> [ExpressionNode] -> ExpressionNode -> ExpressionResult
expandMacro state argNames args body
  | length argNames /= length args = EError("Supplied arguments don't match macro arity")
  | otherwise = evaluateExpression state $ reifyMacroArguments body argNames (map (evaluateExpression state) args) 

potentialMacro :: EvaluationState -> String -> [ExpressionNode] -> ExpressionResult
potentialMacro (EvaluationState macros (i0, i1, i2, i3)) name argValues = potentialMacroCheck $ Map.lookup name macros
  where
    potentialMacroCheck (Just (Macro _ argNames macroBody)) = expandMacro (EvaluationState macros (i0, i1, i2, i3 + 1)) argNames argValues macroBody
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

readMacroDefinitions :: [Token] -> ([Token], [Token], Map.Map String Macro)
readMacroDefinitions [] = ([], [], Map.empty)
readMacroDefinitions (BeginExpression:nr) = (x, y, z)
  where
    (x, y, z) = checkForMacroDefinition $ readMacroDefinition (BeginExpression:nr)
    checkForMacroDefinition (Just x, skip) = (next, tokens, checkForDuplicateDefinition x defs)
      where
        (next, tokens, defs) = readMacroDefinitions skip
        checkForDuplicateDefinition :: Macro -> Map.Map String Macro -> Map.Map String Macro
        checkForDuplicateDefinition (Macro name body rem) d
          | Map.member name d = error ("Duplicate macro definition: " ++ name)
          | otherwise = Map.insert name (Macro name body rem) d
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
tokenify x = []

errorCheck :: (Map.Map String Macro) -> [Token] -> Maybe ExpressionResult
errorCheck macros (BeginExpression:xr) = errorCheckInternal $ evaluateExpression (EvaluationState macros (0,0,0,0)) l
  where
    (l, r) = readExpression (BeginExpression:xr)
    errorCheckInternal (EError e) = Just (EError e)
    errorCheckInternal x = errorCheck macros r
errorCheck macros (x:xr) = errorCheck macros xr
errorCheck macros [] = Nothing

expandMacros :: EvaluationState -> [Token] -> [Token]
expandMacros (EvaluationState m (i0, i1, i2, i3)) c
  | (c /= result) = expandMacros (EvaluationState merged_macros (i0, i1 + 1, i2, i3)) result
  | otherwise = respondToError c $ errorCheck merged_macros c
  where
    result = evaluateMacros 0 code
    merged_macros = Map.union m macros
    (_, code, macros) = readMacroDefinitions c
    evaluateMacros iv (BeginExpression:xr) = deferEvaluation value
      where
        deferEvaluation (EError _) = l ++ evaluateMacros (iv + 1) r where (l, r) = readExpressionLiteral (BeginExpression:xr)
        deferEvaluation x = (tokenify x) ++ evaluateMacros (iv + 1) remainder
        value = evaluateExpression (EvaluationState merged_macros (i0, i1, iv, i3)) expression
        (expression, remainder) = readExpression (BeginExpression:xr)
    evaluateMacros iv [] = []
    evaluateMacros iv (x:xr) = x : evaluateMacros iv xr

    respondToError x (Just (EError e)) = error ("EVALUATION ERROR: " ++ e)
    respondToError x Nothing = x