module Macros where

import Tokenizer
import Expression
import Evaluation
import Debug.Trace

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
tokenify x = trace ("UNDEFINED: " ++ show x) []

expandMacros :: [Token] -> [Token]
expandMacros c = evaluateMacros code
  where
    (_, code, macros) = readMacroDefinitions c
    evaluateMacros (BeginExpression:xr) = (tokenify $ evaluateExpression macros expression) ++ evaluateMacros remainder
      where
        (expression, remainder) = readExpression (BeginExpression:xr)
    evaluateMacros [] = []
    evaluateMacros (x:xr) = x : evaluateMacros xr