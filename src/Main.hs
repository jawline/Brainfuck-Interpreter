import System.Environment 
import Data.Char (isSpace)
import State (initialState)
import Interpreter (interpret)
import Ast (parseProgram)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main = do
  args <- getArgs
  contents <- readFile (head args)
  let parsed = parseProgram (trim contents)
  interpret initialState parsed
