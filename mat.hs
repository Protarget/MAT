import System.Environment
import Numeric
import Assembler

main = do
  cmdArgs <- getArgs
  fileData <- readFile (cmdArgs !! 0)
  putStrLn $ show $ map (\(AssemblyFragment a v) -> (showHex a "") ++ ": " ++ (show $ map (\n -> showHex n "") v)) $ assemble fileData
