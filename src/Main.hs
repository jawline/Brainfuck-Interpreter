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
  putStrLn contents
  let parsed = parseProgram (trim contents)
  putStrLn (show parsed)
  interpret initialState parsed
