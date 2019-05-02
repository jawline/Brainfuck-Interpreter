import System.Environment 

data Ast = Get | Put | While AstList | Add | Sub | Next | Last
  deriving (Eq,Show)

type AstList = [Ast]

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
  let parsed = parseProgram contents
  putStrLn (show parsed)
