import System.Environment
import Numeric
import Assembler

main = do
  cmdArgs <- getArgs
  fileData <- readFile (cmdArgs !! 0)
  putStrLn $ show $ map (\n -> showHex n "") $ assemble fileData
