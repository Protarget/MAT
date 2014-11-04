module PTokenizer where

import Text.Parsec
import Text.Parsec.String
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
  TokenLabelX String |
  TokenLabelY String |
  TokenPragma String |
  TokenDefer String |
  TokenByte Int |
  BeginExpression | 
  EndExpression |
  TokenLiteralBegin |
  TokenLiteralEnd |
  TokenComment
  deriving(Eq)

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
  show (TokenDefer v) = "{ERROR:" ++ v ++ " }"
  show (TokenByte v) = ("'" ++ (show v))
  show (TokenLabelX v) = v ++ ",X"
  show (TokenLabelY v) = v ++ ",Y" 
  show BeginExpression = "<"
  show EndExpression = ">"
  show TokenLiteralBegin = "{"
  show TokenLiteralEnd = "}"

tokenizeDec :: Parser Int
tokenizeDec = do
  v <- many1 $ choice [digit, char '-']
  return $ read v

tokenizeHex :: Parser Int
tokenizeHex = do
  char '$'
  v <- many1 hexDigit
  return $ fst $ head $ readHex v

tokenizeNum :: Parser Int
tokenizeNum = choice [tokenizeDec, tokenizeHex]

tokenizeByte :: Parser Token
tokenizeByte = do
  char '`'
  v <- tokenizeNum
  return $ TokenByte v

tokenizeLiteral :: Parser Token
tokenizeLiteral = do
  char '#'
  v <- tokenizeNum
  return $ TokenLiteral v

tokenizeAddress :: Parser Token
tokenizeAddress = do
  v <- tokenizeNum
  return $ TokenAddress v

tokenizeIndirect :: Parser Token
tokenizeIndirect = do
  char '('
  (TokenAddress a) <- tokenizeAddress
  char ')'
  return $ TokenIndirect a

tokenizeIndirectIndexed :: Parser Token
tokenizeIndirectIndexed = do
  char '('
  (TokenAddress y) <- tokenizeAddress
  char ')'
  char ','
  oneOf "yY"
  return $ TokenIndirectIndexed y

tokenizeIndexedIndirect :: Parser Token
tokenizeIndexedIndirect = do
  char '('
  (TokenAddress x) <- tokenizeAddress
  char ','
  oneOf "xX"
  char ')'
  return $ TokenIndexedIndirect x

tokenizeAddressX :: Parser Token
tokenizeAddressX = do
  (TokenAddress x) <- tokenizeAddress
  char ','
  oneOf "xX"
  return $ TokenAddressX x

tokenizeAddressY :: Parser Token
tokenizeAddressY = do
  (TokenAddress y) <- tokenizeAddress
  char ','
  oneOf "yY"
  return $ TokenAddressY y

tokenizeLabelX :: Parser Token
tokenizeLabelX = do
  (TokenSymbol s) <- tokenizeSymbol
  char ','
  oneOf "xX"
  return $ TokenLabelX s

tokenizeLabelY :: Parser Token
tokenizeLabelY = do
  (TokenSymbol s) <- tokenizeSymbol
  char ','
  oneOf "yY"
  return $ TokenLabelY s

tokenizeAddressGroup :: Parser Token
tokenizeAddressGroup = choice [try tokenizeIndirectIndexed, try tokenizeIndexedIndirect, try tokenizeIndirect, try tokenizeAddressX, try tokenizeAddressY, tokenizeAddress]

tokenizePragma :: Parser Token
tokenizePragma = do
  char '.'
  (TokenSymbol x) <- tokenizeSymbol
  return $ TokenPragma x

tokenizeSymbol :: Parser Token
tokenizeSymbol = do
  a <- choice [letter, oneOf "+-*><=%_"]
  b <- many $ choice [letter, oneOf "+-*><=%", digit]
  return $ TokenSymbol (a:b)

tokenizeLabel :: Parser Token
tokenizeLabel = do
    (TokenSymbol a) <- tokenizeSymbol
    _ <- char ':'
    return $ TokenLabel a

tokenizeSymbolic :: Parser Token
tokenizeSymbolic = choice [try tokenizeLabelX, try tokenizeLabelY, try tokenizeLabel, tokenizeSymbol]

tokenizeBeginExpression :: Parser Token
tokenizeBeginExpression = do
  _ <- char '['
  return $ BeginExpression

tokenizeEndExpression :: Parser Token
tokenizeEndExpression = do
  _ <- char ']'
  return $ EndExpression

tokenizeBeginLiteral :: Parser Token
tokenizeBeginLiteral = do
  _ <- char '{'
  return $ TokenLiteralBegin

tokenizeEndLiteral :: Parser Token
tokenizeEndLiteral = do
  _ <- char '}'
  return $ TokenLiteralEnd

tokenizeControlChar :: Parser Token
tokenizeControlChar = choice [tokenizeBeginLiteral, tokenizeEndLiteral, tokenizeBeginExpression, tokenizeEndExpression]

tokenizeString :: Parser Token
tokenizeString = do
  char '"'
  v <- many (noneOf "\"")
  char '"'
  return $ TokenString v

tokenizeComment :: Parser Token
tokenizeComment = do
  char ';'
  v <- many (noneOf "\n")
  string "\n"
  return $ TokenComment

tokenizeToken :: Parser Token
tokenizeToken = choice [tokenizeComment, tokenizePragma, tokenizeString, tokenizeControlChar, tokenizeSymbolic, tokenizeByte, tokenizeLiteral, tokenizeAddressGroup]

tokenizer :: Parser [Token]
tokenizer = do
  spaces
  v <- (endBy tokenizeToken spaces)
  eof
  return $ v

tokenize :: String -> [Token]
tokenize = filter (not . isComment) . checkResult . parse tokenizer ""
  where
    checkResult (Right tokens) = tokens
    checkResult (Left e) = error (show e)

isComment :: Token -> Bool
isComment TokenComment = True
isComment _ = False
