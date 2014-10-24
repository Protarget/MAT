import Macros
import Expression
import Evaluation
import Tokenizer

assemble = expandMacros . tokenize

main = putStrLn "Teh"