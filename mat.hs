import System.Environment
import Numeric
import Evaluation
import Assembler
import PTokenizer
import Data.List
import Data.Maybe
import Debug.Trace
import Data.Char (ord)
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
      (_, tokens) <- tokenizeFile (return i) []
      let result = assemble tokens in
        if o == "" then
          putStrLn $ show result
        else
          BS.writeFile o $ BS.pack result

runApp (AppSettings i o Expand) =
    do
      (_, tokens) <- tokenizeFile (return i) []
      let result = expandMacros newEvaluationState tokens in
        putStrLn $ formatAssembly result result


tokenizeFile :: IO String -> [String] -> IO ([String], [Token])
tokenizeFile filename imported = do
  fileName <- filename
  fileData <- readFile fileName
  scanIncludes (tokenize fileData) imported

tokenLength :: IO [Token] -> IO Int
tokenLength d = do
  x <- d
  return $ length x

scanIncludes :: [Token] -> [String] -> IO ([String], [Token])
scanIncludes ((TokenPragma "include"):(TokenString v):r) imported = do
  (sub, f) <- tokenizeFile (return v) imported
  (sub2, next) <- scanIncludes r (union sub imported)
  return $ (sub2, f ++ next)

scanIncludes ((TokenPragma "import"):(TokenString v):r) imported
  | v `elem` imported = scanIncludes r imported
  | otherwise = do
    (sub, f) <- tokenizeFile (return v) (v:imported)
    (sub2, next) <- scanIncludes r (union (v:imported) sub)
    return $ (sub2, f ++ next) 

scanIncludes ((TokenPragma "incbin"):(TokenString v):r) imported = do
  contents <- BS.readFile v
  (sub, next) <- scanIncludes r imported
  return $ (union sub imported, (map (\v -> TokenByte (fromIntegral v)) (BS.unpack contents) ++ next))

scanIncludes (x:r) imported = do
  (sub, next) <- scanIncludes r imported
  return $ (union sub imported, x : next)

scanIncludes [] _ = return ([], [])

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
