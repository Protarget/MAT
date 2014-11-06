module Expression where

import PTokenizer
import Data.List
import Debug.Trace

data ExpressionNode = ExpressionValue Token | Expression [ExpressionNode] | ExpressionTokenLiteral [Token] | ExpressionError String | ExpressionRaw ExpressionResult deriving (Eq)
data ExpressionResult = EBool Bool | EInt Int | ETokens [Token] | EString String | EVoid | EError String | ELambda [String] ExpressionNode | EList [ExpressionResult] deriving (Eq)
data Macro = Macro String [String] ExpressionNode Bool deriving(Show)

instance Show ExpressionNode where
  show (ExpressionValue v) = show v
  show (Expression v) = show v
  show (ExpressionError v) = "{ERROR: " ++ v ++ "}"
  show (ExpressionTokenLiteral v) = "{TOKENS: " ++ (intercalate " " $ map show v) ++ "}"
  show (ExpressionRaw v) = "{VAL: " ++ show v ++ "}"

instance Show ExpressionResult where
  show (EBool v) = show v
  show (EInt v) = show v
  show (ETokens v) = "[" ++ (intercalate " " $ map show v) ++ "]"
  show (EList v) = "(" ++ (intercalate ", " $ map show v) ++ ")"
  show (EString v) = show v
  show (EVoid) = "{VOID}"
  show (EError v) = "{ERROR: " ++ v ++ "}"
  show (ELambda a b) = "{LAMBDA: " ++ (show b) ++ "}"

readExpression :: [Token] -> (ExpressionNode, [Token])
readExpression (BeginExpression:e) = respondToError x
  where
    (x, y) = scanExpression e
    scanExpression :: [Token] -> ([ExpressionNode], [Token])
    scanExpression (BeginExpression:e) = (v : nv, nr)
      where 
        (v, r)   = readExpression (BeginExpression:e)
        (nv, nr) = scanExpression(r)
    scanExpression (EndExpression:r) = ([], r)
    scanExpression (TokenLiteralBegin:r) = readTokenLiteral 1 r []
    scanExpression [] = (ExpressionError ("Unmatched expression branch at: [" ++ (intercalate [] $ map show e)) : [], [])
    scanExpression ((TokenSymbol "true"):r) = ((ExpressionRaw (EBool True)):nv, nr) where (nv, nr) = scanExpression r
    scanExpression ((TokenSymbol "false"):r) = ((ExpressionRaw (EBool False)):nv, nr) where (nv, nr) = scanExpression r
    scanExpression ((TokenSymbol "void"):r) = ((ExpressionRaw EVoid):nv, nr) where (nv, nr) = scanExpression r
    scanExpression (v:r) = (ExpressionValue v : nv, nr)
      where
        (nv, nr) = scanExpression r
    readTokenLiteral :: Int -> [Token] -> [Token] -> ([ExpressionNode], [Token])
    readTokenLiteral depth (TokenLiteralBegin:r) buffer = readTokenLiteral (depth + 1) r (buffer ++ [TokenLiteralBegin])
    readTokenLiteral 1 (TokenLiteralEnd:r) buffer = (ExpressionTokenLiteral buffer : nv, nr)
      where
        (nv, nr) = scanExpression r
    readTokenLiteral depth (TokenLiteralEnd:r) buffer = readTokenLiteral (depth - 1) r (buffer ++ [TokenLiteralEnd])
    readTokenLiteral depth (v:r) buffer = readTokenLiteral depth r (buffer ++ [v])
    readTokenLiteral depth [] buffer = ((ExpressionError "Missing end to token literal"):[], [])

    respondToError (ExpressionError e:_) = error ("PARSE ERROR: " ++ e)
    respondToError (v:r) = respondToError r
    respondToError [] = (Expression x, y)

readExpression (x:xr) = (ExpressionError ("Unable to parse expression branch:" ++ show x), xr)

readExpressionLiteral :: [Token] -> ([Token], [Token])
readExpressionLiteral = readExpressionLiteralDepth 0
  where
    readExpressionLiteralDepth :: Int -> [Token] -> ([Token], [Token])
    readExpressionLiteralDepth depth (BeginExpression:r) = ((BeginExpression:next), rem) where (next, rem) = readExpressionLiteralDepth (depth + 1) r
    readExpressionLiteralDepth 1     (EndExpression:r) = ((EndExpression:[]), r)
    readExpressionLiteralDepth depth (EndExpression:r) = ((EndExpression:next), rem) where  (next, rem) = readExpressionLiteralDepth (depth - 1) r
    readExpressionLiteralDepth depth (n:r) = (n:next, rem) where  (next, rem) = readExpressionLiteralDepth depth r
    readExpressionLiteralDepth x y = error ("Wtf: " ++ show x ++ " | " ++ show y)


expressionErrors :: ExpressionNode -> [ExpressionNode]
expressionErrors (ExpressionError v) = [ExpressionError v]
expressionErrors (ExpressionValue v) = []
expressionErrors (Expression v) = intercalate [] $ map expressionErrors v

expressionValid :: ExpressionNode -> Maybe ExpressionNode
expressionValid expr
  | (null $ expressionErrors expr) = Just expr
  | otherwise = Nothing
