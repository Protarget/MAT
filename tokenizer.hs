module Tokenizer where

import Text.Regex
import Data.Maybe
import Debug.Trace
import Data.Char
import Data.String
import Data.List
import Data.Ord
import Numeric

data Token = 
  TokenLabel String | 
  TokenSymbol String | 
  TokenString String | 
  TokenLiteral Int | 
  TokenAddress Int |
  TokenAddressX Int |
  TokenAddressY Int |
  TokenIndirect Int |
  TokenIndirectIndexed Int |
  TokenIndexedIndirect Int |
  TokenPragma String |
  BeginExpression | 
  EndExpression |
  TokenLiteralDelimiter

type TokenProducer = [String] -> (Token, String)
type TokenCandidate = (Maybe (String, String, String, [String]), TokenProducer, Int)
type TokenDefinition = (Regex, TokenProducer, Int)

instance Show Token where
  show (TokenLabel v) = v ++ ":"
  show (TokenSymbol v) = v
  show (TokenString v) = '"':v ++ "\""
  show (TokenLiteral v) = '#' : show v
  show (TokenAddress v) = show v
  show (TokenAddressX v) = show v ++ ",X"
  show (TokenAddressY v) = show v ++ ",Y"
  show (TokenIndirect v) = '(':(show v) ++ ")"
  show (TokenIndirectIndexed v) = '(':(show v) ++ "),Y"
  show (TokenIndexedIndirect v) = '(':(show v) ++ ",X)"
  show (TokenPragma v) = '.' : (show v)
  show BeginExpression = "<"
  show EndExpression = ">"
  show TokenLiteralDelimiter = "`"

thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x  _ = x

defineToken :: String -> TokenProducer -> Int -> (Regex, TokenProducer, Int)
defineToken pat fn pri = (mkRegex pat, fn, pri)

readNumber :: String -> Int
readNumber ('$':s) = fst $ head $ readHex s
readNumber s = read s :: Int

readAddress :: TokenProducer
readAddress (x:y:[]) = (TokenAddress (readNumber x), y)
readAddress x = error ("Cannot read address from " ++ show x)

readLiteral :: TokenProducer
readLiteral (x:y:[]) = (TokenLiteral (readNumber x), y)
readLiteral x = error ("Cannot read literal from " ++ show x)

readIndirect :: TokenProducer
readIndirect (x:[]) = (TokenIndirect (readNumber x), "")

readIndirectIndexed :: TokenProducer
readIndirectIndexed (x:[]) = (TokenIndirectIndexed (readNumber x), "")

readIndexedIndirect :: TokenProducer
readIndexedIndirect (x:[]) = (TokenIndexedIndirect (readNumber x), "")

readControl :: TokenProducer
readControl ("[":[]) = (BeginExpression, "")
readControl ("]":[]) = (EndExpression, "")
readControl ("`":[]) = (TokenLiteralDelimiter, "")

readSymbol :: TokenProducer
readSymbol (x:y:xs) = (TokenSymbol x, y)

readString :: TokenProducer
readString (x:_) = (TokenString x, "")

readLabel :: TokenProducer
readLabel (x:y:xs) = (TokenLabel x, y)

readPragma :: TokenProducer
readPragma (x:y:xs) = (TokenPragma x, y)

readAddressX :: TokenProducer
readAddressX (x:_) = (TokenAddressX (readNumber x), "")

readAddressY :: TokenProducer
readAddressY (x:_) = (TokenAddressY (readNumber x), "")

tokenDefinitions :: [TokenDefinition]
tokenDefinitions =
  (defineToken "\"(.*)\""                                  readString 1)          :    -- Read a quoted string
  (defineToken "\\.([a-zA-Z]+)([^a-zA-Z]|$)"               readPragma 1)          :    -- Read a pragma
  (defineToken "([0-9]+)\\,[xX]"                           readAddressX 2)        :    -- Read a decimal,X address
  (defineToken "(\\$[0-9a-fA-F]+)\\,[xX]"                  readAddressX 2)        :    -- Read a hex,X address
  (defineToken "([0-9]+)\\,[yY]"                           readAddressY 2)        :    -- Read a decimal,Y address
  (defineToken "(\\$[0-9a-fA-F]+)\\,[yY]"                  readAddressY 2)        :    -- Read a hex,Y address
  (defineToken "\\(([0-9]+)\\),[yY]"                       readIndirectIndexed 3) :    -- Read a decimal indirect indexed address
  (defineToken "\\((\\$[0-9a-fA-F]+)\\)\\,[yY]"            readIndirectIndexed 3) :    -- Read a hex indirect indexed address
  (defineToken "\\(([0-9]+)\\,[xX]\\)"                     readIndexedIndirect 3) :    -- Read a decimal indexed indirect address
  (defineToken "\\((\\$[0-9a-fA-F]+)\\,[xX]\\)"            readIndexedIndirect 3) :    -- Read a hex indexed indirect address
  (defineToken "\\(([0-9]+)\\)"                            readIndirect 4)        :    -- Read a decimal indirect address
  (defineToken "\\((\\$[0-9a-fA-F]+)\\)"                   readIndirect 4)        :    -- Read a hex indirect address
  (defineToken "([0-9]+)([^0-9]|$)"                        readAddress 5)         :    -- Read a decimal address
  (defineToken "(\\$[0-9a-fA-F]+)([^0-9a-fA-F]|$)"         readAddress 5)         :    -- Read a hex address
  (defineToken "\\#([0-9]+)([^0-9]|$)"                     readLiteral 5)         :    -- Read a decimal literal
  (defineToken "\\#(\\$[0-9a-fA-F]+)([^0-9a-fA-F]|$)"      readLiteral 5)         :    -- Read a hex literal
  (defineToken "(\\[|\\]|`)"                               readControl 5)         :    -- Read a control character
  (defineToken "([a-zA-Z+-*][a-zA-Z0-9]*)\\:([^\\:]|$)"    readLabel 4)           :    -- Read a label
  (defineToken "([a-zA-Z][a-zA-Z0-9]*)([^a-zA-Z0-9]|$)"    readSymbol 5)          : [] -- Read a symbol

tokenizeEmit :: TokenCandidate -> [Token]
tokenizeEmit (Just (before, _, after, subexpressions), fn, _) = token : (tokenize (ignored ++ after))
  where 
    (token, ignored) = fn subexpressions

tokenizeShortestCompare :: TokenCandidate -> TokenCandidate -> Ordering
tokenizeShortestCompare (Just (a,_,_,_), _, pa) (Just (b,_,_,_), _, pb) = comparing (length) a b `thenCmp` compare pa pb
tokenizeShortestCompare (Just (a,_,_,_), _, _) _ = LT
tokenizeShortestCompare _ (Just (b,_,_,_), _, _) = GT
tokenizeShortestCompare _ _ = GT

tokenizeShortest :: [TokenCandidate] -> TokenCandidate
tokenizeShortest x  = minimumBy (tokenizeShortestCompare) x

tokenize :: String -> [Token]
tokenize "" = []
tokenize x = tokenizeEmit $ tokenizeShortest matches
  where   
    matches = filter (\(n, _, _) -> isJust n) $ map (\(n, y, p) -> (matchRegexAll n x, y, p)) tokenDefinitions