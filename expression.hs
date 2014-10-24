module Expression where

import Tokenizer
import Data.List

data ExpressionNode = ExpressionValue Token | Expression [ExpressionNode] | ExpressionTokenLiteral [Token] | ExpressionError String | ExpressionRaw ExpressionResult
data ExpressionResult = EBool Bool | EInt Int | ETokens [Token] | EString String | EVoid | EError String
data Macro = Macro String [String] ExpressionNode deriving(Show)

instance Show ExpressionNode where
  show (ExpressionValue v) = show v
  show (Expression v) = show v
  show (ExpressionError v) = "{ERROR: " ++ v ++ "}"
  show (ExpressionTokenLiteral v) = "`" ++ (intercalate " " $ map show v) ++ "`"
  show (ExpressionRaw v) = "{VAL: " ++ show v ++ "}"

instance Show ExpressionResult where
  show (EBool v) = show v
  show (EInt v) = show v
  show (ETokens v) = "[" ++ (intercalate " " $ map show v) ++ "]"
  show (EString v) = show v
  show (EVoid) = "{VOID}"
  show (EError v) = "{ERROR: " ++ v ++ "}"

readExpression :: [Token] -> (ExpressionNode, [Token])
readExpression (BeginExpression:e) = (Expression x, y)
  where
    (x, y) = scanExpression e
    scanExpression :: [Token] -> ([ExpressionNode], [Token])
    scanExpression (BeginExpression:e) = (v : nv, nr)
      where 
        (v, r)   = readExpression (BeginExpression:e)
        (nv, nr) = scanExpression(r)
    scanExpression (EndExpression:r) = ([], r)
    scanExpression (TokenLiteralDelimiter:r) = readTokenLiteral r []
    scanExpression [] = (ExpressionError ("Unmatched expression branch at: [" ++ (intercalate [] $ map show e)) : [], [])
    scanExpression ((TokenSymbol "true"):r) = ((ExpressionRaw (EBool True)):nv, nr) where (nv, nr) = scanExpression r
    scanExpression ((TokenSymbol "false"):r) = ((ExpressionRaw (EBool False)):nv, nr) where (nv, nr) = scanExpression r
    scanExpression ((TokenSymbol "void"):r) = ((ExpressionRaw EVoid):nv, nr) where (nv, nr) = scanExpression r
    scanExpression (v:r) = (ExpressionValue v : nv, nr)
      where
        (nv, nr) = scanExpression r
    readTokenLiteral :: [Token] -> [Token] -> ([ExpressionNode], [Token])
    readTokenLiteral (TokenLiteralDelimiter:r) buffer = (ExpressionTokenLiteral buffer : nv, nr)
      where
        (nv, nr) = scanExpression r
    readTokenLiteral (v:r) buffer = readTokenLiteral r (buffer ++ [v])

readExpression (x:xr) = (ExpressionError ("Unable to parse expression branch:" ++ show x), xr)

expressionErrors :: ExpressionNode -> [ExpressionNode]
expressionErrors (ExpressionError v) = [ExpressionError v]
expressionErrors (ExpressionValue v) = []
expressionErrors (Expression v) = intercalate [] $ map expressionErrors v

expressionValid :: ExpressionNode -> Maybe ExpressionNode
expressionValid expr
  | (length $ expressionErrors expr) == 0 = Just expr
  | otherwise = Nothing