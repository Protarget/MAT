import System.Environment
import Numeric
import Evaluation
import Assembler
import Tokenizer
import Debug.Trace
import Data.List

data AppMode = Expand | Assemble deriving(Show)
data AppSettings = AppSettings String AppMode deriving(Show)

main :: IO ()
main = do
  cmdArgs <- getArgs
  runApp $ collectMode cmdArgs (AppSettings "" Assemble)

collectMode :: [String] -> AppSettings -> AppSettings
collectMode ("-i":fn:r) (AppSettings _ m) =  collectMode r (AppSettings fn m)
collectMode ("-e":r) (AppSettings fn _) =  collectMode r (AppSettings fn Expand)
collectMode ("-a":r) (AppSettings fn _) =  collectMode r (AppSettings fn Assemble)
collectMode [] x = x
 
runApp :: AppSettings -> IO ()
runApp (AppSettings fn Assemble) =
    do
      tokens <-  tokenizeFile $ return fn
      let result = assembleTokenizedInput tokens in
        putStrLn $ show $ map (\(AssemblyFragment a v) -> (showHex a "") ++ ": " ++ (show $ map (\n -> showHex n "") v)) $ result

runApp (AppSettings fn Expand) =
    do
      tokens <-  tokenizeFile $ return fn
      let result = expandMacros [] tokens in
        putStrLn $ intercalate "\n" $ map show result


tokenizeFile :: IO String -> IO [Token]
tokenizeFile filename = do
  fileName <- filename
  fileData <- readFile fileName
  scanIncludes $ tokenize fileData

scanIncludes :: [Token] -> IO [Token]
scanIncludes ((TokenPragma "include"):(TokenString f):r) = do
  f <- tokenizeFile $ return f
  next <- scanIncludes r
  return $ f ++ next

scanIncludes (x:r) = do
  next <- scanIncludes r
  return $ x : next

showAssembly :: [Token] -> String
showAssembly ((TokenSymbol x):(TokenSymbol y):r)
showAssembly [] = ""

scanIncludes [] = return []