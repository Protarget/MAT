module Evaluation where

import PTokenizer
import Expression
import Data.List
import Data.Maybe
import Data.Bits
import Debug.Trace
import qualified Data.Map as Map
import Data.Char (ord, chr)

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

isExpressionError :: ExpressionResult -> Bool
isExpressionError (EError _) = True
isExpressionError _ = False

isNodeSymbol :: ExpressionNode -> Bool
isNodeSymbol (ExpressionValue (TokenSymbol _)) = True
isNodeSymbol _ = False

isVariadic :: [ExpressionNode] -> Bool
isVariadic args
  | length args == 0 = False
  | last args == (ExpressionValue (TokenSymbol "_")) = True
  | otherwise = False

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
    ifChecker (n) (a:b:[]) = trace (show n ++ " " ++ show a ++ " " ++ show b) $ conditionalError [n] "If statement first argument must be boolean"
builtinIf state _ = EError "If must be 3-arity"

builtinEqual :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinEqual state (a:b:[]) = equalChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    equalChecker x y = EBool (x == y)
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
builtinAddrX = builtinNumericTokenizer (\x -> ETokens [TokenAddressX x]) "addrx" "Cannot create addressX token from non-integer value"
builtinAddrY = builtinNumericTokenizer (\x -> ETokens [TokenAddressY x]) "addry" "Cannot create addressY token from non-integer value"
builtinByte = builtinNumericTokenizer (\x -> ETokens [TokenByte x]) "byte" "Cannot create byte token from non-integer value"
builtinString = builtinStringTokenizer (\x -> ETokens [TokenString x]) "string" "Cannot create a string token from non-string value"
builtinInd = builtinNumericTokenizer (\x -> ETokens [TokenIndirect x]) "ind" "Cannot create indirect token from non-integer value"
builtinInIx = builtinNumericTokenizer (\x -> ETokens [TokenIndirectIndexed x]) "inix" "Cannot create indirect indexed token from non-integer value"
builtinIxIn = builtinNumericTokenizer (\x -> ETokens [TokenIndexedIndirect x]) "ixin" "Cannot create indexed indirect token from non-integer value"
builtinSym = builtinStringTokenizer(\x -> ETokens [TokenSymbol x]) "sym" "Cannot create symbol from non-string value"
builtinLabel = builtinStringTokenizer(\x -> ETokens [TokenLabel x]) "label" "Cannot create label from non-string value"
builtinLabelX = builtinStringTokenizer(\x -> ETokens [TokenLabelX x]) "labelx" "Cannot create labelX from non-string value"
builtinLabelY = builtinStringTokenizer(\x -> ETokens [TokenLabelY x]) "labely" "Cannot create labelY from non-string value"
builtinPragma = builtinStringTokenizer(\x -> ETokens [TokenPragma x]) "pragma" "Cannot create pragma from non-string value"

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

builtinBOr = builtinNumericFold (.|.) "Cannot bitwise-or non-integer values"
builtinBAnd = builtinNumericFold (.&.) "Cannot bitwise-and non-integer values"
builtinBXor = builtinNumericFold xor "Cannot bitwise-xor non-integer values"

builtinBNot :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinBNot state (a:[]) = bNotChecker (evaluateExpression state a)
  where
    bNotChecker (EInt v) = EInt $ complement v
    bNotChecker x = conditionalError [x] "Argument to bnot must be an integer"

builtinLeftShift :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinLeftShift state (a:b:[]) = bLeftShiftChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    bLeftShiftChecker (EInt v) (EInt x) = EInt $ shiftL v x
    bLeftShiftChecker x y = conditionalError [x, y] "Arguments to left shift must be integers"

builtinRightShift :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinRightShift state (a:b:[]) = bRightShiftChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    bRightShiftChecker (EInt v) (EInt x) = EInt $ shiftR v x
    bRightShiftChecker x y = conditionalError [x, y] "Arguments to right shift must be integers"

builtinList :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinList state args
  | any isExpressionError result = conditionalError result "List constructor failed"
  | otherwise = EList result
  where result = map (evaluateExpression state) args

builtinHead :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinHead state (a:[]) = headChecker (evaluateExpression state a)
  where
    headChecker (EString v) = EString [(head v)]
    headChecker (EList v) = (head v)
    headChecker (ETokens v) = ETokens [(head v)]
    headChecker x = conditionalError [x] "Argument to head must be a list, tokens or a string"
builtinHead state _ = EError "Head must be 1-arity"

builtinTail :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinTail state (a:[]) = tailChecker (evaluateExpression state a)
  where
    tailChecker (EString v) = EString (tail v)
    tailChecker (EList v) = EList (tail v)
    tailChecker (ETokens v) = ETokens (tail v)
    tailChecker x = conditionalError [x] "Argument to head must be a list, tokens or a string"
tailChecker state _ = EError "Head must be 1-arity"

builtinCons :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinCons state (a:b:[]) = consChecker (evaluateExpression state a) (evaluateExpression state b)
  where
    consChecker (EError x) (EList v) = (EError x)
    consChecker (EString (x:[])) (EString y) = EString (x:y)
    consChecker (ETokens (x:[])) (ETokens y) = ETokens (x:y)
    consChecker x (EList v) = EList (x:v)
    consChecker x y = conditionalError [x, y] "Arguments to cons must be a value and list, a char and a string, or a token and a tokens"
builtinCons state _ = EError "Cons must be 2-arity"

builtinEmpty :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinEmpty state (a:[]) = emptyChecker (evaluateExpression state a)
  where
    emptyChecker (EList v) = EBool $ null v
    emptyChecker (EString v) = EBool $ null v
    emptyChecker (ETokens v) = EBool $ null v
    emptyChecker x = conditionalError [x] "Argument to empty must be a list, tokens, or a string"
builtinEmpty state _ = EError "Empty must be 1-arity"

builtinLength :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinLength state (a:[]) = lengthChecker (evaluateExpression state a)
  where
    lengthChecker (EList v) = EInt $ length v
    lengthChecker (EString v) = EInt $ length v
    lengthChecker (ETokens v) = EInt $ length v
    lengthChecker x = conditionalError [x] "Argument to length must be a list, tokens, or a string"
builtinLength state _ = EError "Length must be 1-arity"

builtinOrd :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinOrd state (a:[]) = ordChecker (evaluateExpression state a)
  where
    ordChecker (EString (v:[])) = EInt $ ord v
    ordChecker x = conditionalError [x] "Argument to ord must be a single character"
builtinOrd state _ = EError "Ord must be 1-arity"

builtinChr :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinChr state (a:[]) = chrChecker (evaluateExpression state a)
  where
    chrChecker (EInt v) = EString $ [chr v]
    chrChecker x = conditionalError [x] "Argument to chr must be a single integer"
builtinChr state _ = EError "Chr must be 1-arity"

builtinType :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinType state (a:[]) = typeChecker (evaluateExpression state a)
  where
    typeChecker (EInt _) = EString "int"
    typeChecker (EString _) = EString "string"
    typeChecker (EBool _) = EString "bool"
    typeChecker (EError _) = EString "error"
    typeChecker (ETokens _) = EString "tokens"
    typeChecker (ELambda _ _) = EString "lambda"
    typeChecker (EList _) = EString "list"
builtinType state _ = EError "Type must be 1-arity"

builtinTrace :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinTrace state (a:[]) = traceChecker (evaluateExpression state a)
  where
    traceChecker (EString v) = trace v EVoid
    traceChecker v = trace (show v) EVoid
builtinTrace state _ = EError "Trace must be 1-arity"

builtinToString :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinToString state (a:[]) = toStringChecker (evaluateExpression state a)
  where
    toStringChecker (EString x) = EString x
    toStringChecker x = EString $ show x

builtinToInt :: EvaluationState -> [ExpressionNode] -> ExpressionResult
builtinToInt state (a:[]) = toIntChecker (evaluateExpression state a)
  where
    toIntChecker (EString v) = EInt $ read v
    toIntChecker v = conditionalError [v] "Argument to ->int must be convertable to an integer"
 
evaluateExpression :: EvaluationState -> ExpressionNode -> ExpressionResult
evaluateExpression state (Expression (ExpressionValue (TokenSymbol(f)):args)) 
  | f == "merge" = builtinMerge state args
  | f == "&" = builtinMerge state args
  | f == "+" = builtinAdd state args
  | f == "-" = builtinSub state args
  | f == "*" = builtinMul state args
  | f == "lit" = builtinLit state args
  | f == "addr" = builtinAddr state args
  | f == "addrx" = builtinAddrX state args
  | f == "addry" = builtinAddrY state args
  | f == "labelx" = builtinLabelX state args
  | f == "labely" = builtinLabelY state args
  | f == "byte" = builtinByte state args
  | f == "string" = builtinString state args
  | f == "ind" = builtinInd state args
  | f == "inix" = builtinInIx state args
  | f == "ixin" = builtinIxIn state args
  | f == "label" = builtinLabel state args
  | f == "sym" = builtinSym state args
  | f == "pragma" = builtinPragma state args
  | f == "if" = builtinIf state args
  | f == "equal" = builtinEqual state args
  | f == "greater" = builtinGreater state args
  | f == "lesser" = builtinLesser state args
  | f == "or" = builtinOr state args
  | f == "and" = builtinAnd state args
  | f == "not" = builtinNot state args
  | f == "bor" = builtinBOr state args
  | f == "band" = builtinBAnd state args
  | f == "bnot" = builtinBNot state args
  | f == "bxor" = builtinBXor state args
  | f == "<<" = builtinLeftShift state args
  | f == ">>" = builtinRightShift state args
  | f == "expand" = builtinExpand state args
  | f == "let" = builtinLet state args
  | f == "id" = EString ((show i0) ++ "_" ++ (show i1) ++ "_" ++ (show i2) ++ "_" ++ (show i3))
  | f == "lambda" = builtinLambda state args
  | f == "%" = builtinLambda state args
  | f == "apply" = builtinApply state args
  | f == "list" = builtinList state args
  | f == "head" = builtinHead state args
  | f == "tail" = builtinTail state args
  | f == "cons" = builtinCons state args
  | f == "empty" = builtinEmpty state args
  | f == "length" = builtinLength state args
  | f == "ord" = builtinOrd state args
  | f == "chr" = builtinChr state args
  | f == "type" = builtinType state args
  | f == "trace" = builtinTrace state args
  | f == "->int" = builtinToInt state args
  | f == "->string" = builtinToString state args
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
evaluateExpression state node = error ("Cannot execute this expression: " ++ (show node))

reifyMacroArgument :: ExpressionNode -> String -> ExpressionResult -> ExpressionNode
reifyMacroArgument (ExpressionValue (TokenSymbol x)) argName argValue
  | x == argName = ExpressionRaw argValue
  | otherwise = (ExpressionValue (TokenSymbol x))
reifyMacroArgument (Expression v) x y = Expression (map (\n -> reifyMacroArgument n x y) v)
reifyMacroArgument n _ _ = n
 
reifyMacroArguments :: ExpressionNode -> [String] -> [ExpressionResult] -> ExpressionNode
reifyMacroArguments body args values = foldl (\x (n, v) -> reifyMacroArgument x n v) body (zip args values)

expandMacro :: EvaluationState -> String -> [String] -> [ExpressionNode] -> ExpressionNode -> Bool -> ExpressionResult
expandMacro state macroName argNames args body variadic
  | variadic && length args < length argNames = EError (macroName ++": Supplied arguments don't match variadic macro arity") 
  | variadic = let argVals = (map (evaluateExpression state) $ take (length argNames) args) ++ [EList (map (evaluateExpression state) (drop (length argNames) args))]
      in evaluateExpression state $ reifyMacroArguments body (argNames ++ ["args"]) argVals
  | length argNames /= length args = EError(macroName ++ ": Supplied arguments don't match macro arity")
  | otherwise = evaluateExpression state $ reifyMacroArguments body argNames (map (evaluateExpression state) args)

potentialMacro :: EvaluationState -> String -> [ExpressionNode] -> ExpressionResult
potentialMacro (EvaluationState macros (i0, i1, i2, i3)) name argValues = potentialMacroCheck $ Map.lookup name macros
  where
    potentialMacroCheck (Just (Macro _ argNames macroBody variadic)) = expandMacro (EvaluationState macros (i0, i1, i2, i3 + 1)) name argNames argValues macroBody variadic
    potentialMacroCheck Nothing = EError("Undefined macro name: " ++ name)

readMacroDefinition :: [Token] -> (Maybe Macro, [Token])
readMacroDefinition (n:nr) = readMacroDefinition tree
  where
    (tree, remainder) = readExpression (n:nr)
    readMacroDefinition :: ExpressionNode -> (Maybe Macro, [Token])
    readMacroDefinition (Expression ((ExpressionValue (TokenSymbol "macro")) : (ExpressionValue (TokenSymbol macroName) : (Expression macroArgs) : macroBody : [])))
      | isVariadic macroArgs = (Just $ Macro macroName (map (\(ExpressionValue (TokenSymbol v)) -> v) $ init macroArgs) macroBody True, remainder)
      | all isNodeSymbol macroArgs = (Just $ Macro macroName (map (\(ExpressionValue (TokenSymbol v)) -> v) macroArgs) macroBody False, remainder)
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
        checkForDuplicateDefinition (Macro name body rem variadic) d
          | Map.member name d = error ("Duplicate macro definition: " ++ name)
          | otherwise = Map.insert name (Macro name body rem variadic) d
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