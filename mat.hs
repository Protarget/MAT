import System.Environment
import Numeric
import Evaluation
import Assembler
import PTokenizer
import Data.List
import qualified Data.ByteString as BS

data AppMode = Expand | Assemble deriving(Show)
data AppSettings = AppSettings String String AppMode deriving(Show)

main :: IO ()
main = do
  cmdArgs <- getArgs
  runApp $ collectMode cmdArgs (AppSettings "" "" Assemble)

collectMode :: [String] -> AppSettings -> AppSettings
collectMode ("-i":fn:r) (AppSettings _ o m) =  collectMode r (AppSettings fn o m)
collectMode ("-o":fn:r) (AppSettings i _ m) = collectMode r (AppSettings i fn m)
collectMode ("-e":r) (AppSettings i o _) =  collectMode r (AppSettings i o Expand)
collectMode ("-a":r) (AppSettings i o _) =  collectMode r (AppSettings i o Assemble)
collectMode [] x = x
 
runApp :: AppSettings -> IO ()
runApp (AppSettings i o Assemble) =
    do
      tokens <- tokenizeFile $! return i
      let result = assemble tokens in
        if o == "" then
          putStrLn $ show result
        else
          BS.writeFile o $ BS.pack result

runApp (AppSettings i o Expand) =
    do
      tokens <- tokenizeFile $! return i
      let result = expandMacros newEvaluationState tokens in
        putStrLn $ formatAssembly result result


tokenizeFile :: IO String -> IO [Token]
tokenizeFile filename = do
  fileName <- filename
  fileData <- readFile fileName
  scanIncludes $ tokenize fileData

tokenLength :: IO [Token] -> IO Int
tokenLength d = do
  x <- d
  return $ length x

scanIncludes :: [Token] -> IO [Token]
scanIncludes ((TokenPragma "include"):(TokenString f):r) = do
  f <- tokenizeFile $ return f
  next <- scanIncludes r
  return $ f ++ next

scanIncludes (x:r) = do
  next <- scanIncludes r
  return $ x : next

scanIncludes [] = return []

formatAssembly :: [Token] -> [Token] -> String
formatAssembly full ((TokenSymbol t):(TokenSymbol v):r)
  | TokenLabel v `elem` full = t ++ " " ++ v ++ "\n" ++ formatAssembly full r
  | otherwise = t ++ "\n" ++ formatAssembly full ((TokenSymbol v):r)
formatAssembly full ((TokenSymbol t):(TokenByte v):r) = t ++ "\n`" ++ (show v) ++ "\n" ++ formatAssembly full r
formatAssembly full ((TokenByte t):(TokenByte v):r) = ('`':(show t)) ++ " " ++ formatAssembly full ((TokenByte v):r)
formatAssembly full ((TokenSymbol t):(TokenPragma v):r) = t ++ "\n" ++ formatAssembly full ((TokenPragma v):r)
formatAssembly full ((TokenSymbol t):v:r) = t ++ " " ++ (show v) ++ "\n" ++ formatAssembly full r
formatAssembly full ((TokenPragma "org"):(TokenAddress v):r) = ".org " ++ (show v) ++ "\n" ++ formatAssembly full r
formatAssembly full ((TokenPragma "map"):(TokenAddress v):r) = ".map " ++ (show v) ++ "\n" ++ formatAssembly full r
formatAssembly full (x:r) = show x ++ "\n" ++ formatAssembly full r
formatAssembly full [] = ""
