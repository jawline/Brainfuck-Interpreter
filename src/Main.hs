import System.Environment 
import Data.Char (isSpace)

data Ast = Get | Put | While AstList | Add | Sub | Next | Last
  deriving (Eq,Show)

type AstList = [Ast]

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

parseToken :: Char -> Ast
parseToken '>' = Next
parseToken '<' = Last
parseToken '+' = Add
parseToken '-' = Sub
parseToken '.' = Put
parseToken ',' = Get

parsePortion :: String -> (String, AstList)
parsePortion (']':xs) = (xs, [])
parsePortion ('[':xs) = (follows, (While loop):rest)
  where (afterLoop, loop) = parsePortion xs
        (follows, rest) = parsePortion afterLoop
parsePortion (x:xs) = (follows, (parseToken x):rest)
  where (follows, rest) = parsePortion xs

parseProgram :: String -> AstList
parseProgram [] = []
parseProgram ('[':xs) = (While loop):(parseProgram follows)
  where (follows, loop) = parsePortion xs
parseProgram (x:xs) = (parseToken x):parseProgram xs

interpret :: AstList -> IO ()
interpret [] = do return ()
interpret (x:xs) = do
  case x of
    Get -> return ()
    Put -> return ()
    While loopPortion -> return ()
    Add -> return ()
    Sub -> return ()

main = do
  args <- getArgs
  contents <- readFile (head args)
  putStrLn contents
  let parsed = parseProgram (trim contents)
  putStrLn (show parsed)
