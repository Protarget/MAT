module Evaluation where

import Tokenizer
import Expression
import Data.List

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

builtinMerge :: [Macro] -> [ExpressionNode] -> ExpressionResult
builtinMerge macros = mergeChecker . map (evaluateExpression macros) 
  where
    mergeChecker :: [ExpressionResult] -> ExpressionResult
    mergeChecker v
      | all isExpressionTokens v = ETokens $ mergeHelper v
      | otherwise = EError("Cannot merge non-token values")

    mergeHelper :: [ExpressionResult] -> [Token]
    mergeHelper ((ETokens t):r) = t ++ mergeHelper r
    mergeHelper [] = []

builtinNumericFold :: (Int -> Int -> Int) -> String -> [Macro] -> [ExpressionNode] -> ExpressionResult
builtinNumericFold fn err macros = foldChecker . map (evaluateExpression macros)
  where
    foldChecker :: [ExpressionResult] -> ExpressionResult
    foldChecker v
      | all isExpressionInt v = EInt $ foldl1 fn $ map (\(EInt n) -> n) v
      | otherwise = EError(err)

builtinNumericTokenizer :: (Int -> ExpressionResult) -> String -> String -> [Macro] -> [ExpressionNode] -> ExpressionResult
builtinNumericTokenizer fn errn err macros = ntokenizerChecker . map (evaluateExpression macros)
  where
    ntokenizerChecker :: [ExpressionResult] -> ExpressionResult
    ntokenizerChecker v
      | length v /= 1 = EError(errn ++ " must be 1-arity")
      | all isExpressionInt v = fn ((\(EInt n) -> n) $ head v)
      | otherwise = EError(err)

builtInStringTokenizer :: (String -> ExpressionResult) -> String -> String -> [Macro] -> [ExpressionNode] -> ExpressionResult
builtInStringTokenizer fn errn err macros = stokenizerChecker . map (evaluateExpression macros)
  where
    stokenizerChecker :: [ExpressionResult] -> ExpressionResult
    stokenizerChecker v
      | length v /= 1 = EError(errn ++ " must be 1-arity")
      | all isExpressionString v = fn ((\(EString n) -> n) $ head v)
      | otherwise = EError(err)

builtinIf :: [Macro] -> [ExpressionNode] -> ExpressionResult
builtinIf macros (v:a:b:[]) = ifChecker (evaluateExpression macros v) [a, b]
  where
    ifChecker :: ExpressionResult -> [ExpressionNode] -> ExpressionResult
    ifChecker (EBool True) (a:b:[]) = evaluateExpression macros a
    ifChecker (EBool False) (a:b:[]) = evaluateExpression macros b
builtinIf macros _ = EError("If must be 3-arity")

builtinEqual :: [Macro] -> [ExpressionNode] -> ExpressionResult
builtinEqual macros (a:b:[]) = equalChecker (evaluateExpression macros a) (evaluateExpression macros b)
  where
    equalChecker (EInt x) (EInt y) = EBool (x == y)

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

reifyMacroArgument :: ExpressionNode -> String -> ExpressionResult -> ExpressionNode
reifyMacroArgument (ExpressionValue (TokenSymbol x)) argName argValue
  | x == argName = ExpressionRaw argValue
  | otherwise = (ExpressionValue (TokenSymbol x))
reifyMacroArgument (Expression v) x y = Expression (map (\n -> reifyMacroArgument n x y) v)
reifyMacroArgument n _ _ = n

reifyMacroArguments :: ExpressionNode -> [String] -> [ExpressionResult] -> ExpressionNode
reifyMacroArguments body args values = foldl (\x (n, v) -> reifyMacroArgument x n v) body (zip args values)

expandMacro :: [Macro] -> [String] -> [ExpressionNode] -> ExpressionNode -> ExpressionResult
expandMacro macros argNames args body
  | length argNames /= length args = EError("Supplied arguments don't match macro arity")
  | otherwise = evaluateExpression macros $ reifyMacroArguments body argNames (map (evaluateExpression macros) args) 

potentialMacro :: [Macro] -> String -> [ExpressionNode] -> ExpressionResult
potentialMacro macros name argValues = potentialMacroCheck $ find (\(Macro v _ _) -> v == name) macros
  where
    potentialMacroCheck (Just (Macro _ argNames macroBody)) = expandMacro macros argNames argValues macroBody
    potentialMacroCheck Nothing = EError("Undefined macro name: " ++ name)

evaluateExpression :: [Macro] -> ExpressionNode -> ExpressionResult
evaluateExpression macros (Expression (ExpressionValue (TokenSymbol(f)):args)) 
  | f == "value" = builtinExtractValue args
  | f == "merge" = builtinMerge macros args
  | f == "add" = builtinAdd macros args
  | f == "sub" = builtinSub macros args
  | f == "mul" = builtinMul macros args
  | f == "lit" = builtinLit macros args
  | f == "addr" = builtinAddr macros args
  | f == "ind" = builtinInd macros args
  | f == "inix" = builtinInIx macros args
  | f == "ixin" = builtinIxIn macros args
  | f == "label" = builtinLabel macros args
  | f == "sym" = builtinSym macros args
  | f == "if" = builtinIf macros args
  | f == "equal" = builtinEqual macros args
  | otherwise = potentialMacro macros f args


evaluateExpression macros (ExpressionTokenLiteral v) = ETokens v
evaluateExpression macros (ExpressionValue (TokenAddress v)) = EInt v
evaluateExpression macros (ExpressionValue (TokenLiteral v)) = EError("Literal token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenIndirect v)) = EError("Indirect token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenIndexedIndirect v)) = EError("Indexed Indirect token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenIndirectIndexed v)) = EError("Indirect Indexed token is not a valid expression type")
evaluateExpression macros (ExpressionValue (TokenString v)) = EString v
evaluateExpression macros (ExpressionRaw v) = v