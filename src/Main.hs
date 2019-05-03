import System.Environment 
import Data.Char (isSpace)
import Data.Word (Word8)
import Data.Map (Map, findWithDefault, insert, empty)
import Data.Binary.Get (getWord8)
import Unsafe.Coerce (unsafeCoerce)

data Ast = Get | Put | While AstList | Add | Sub | Next | Last
  deriving (Eq,Show)
type AstList = [Ast]

type State = (Integer, Map Integer Word8)

stateGetCurrent :: State -> Word8
stateGetCurrent (currentIndex, memory) = findWithDefault 0 currentIndex memory 

stateSetCurrent :: State -> Word8 -> State
stateSetCurrent (currentIndex, memory) val = (currentIndex, insert currentIndex val memory)

stateNext :: State -> State
stateNext (currentIndex, memory) = (currentIndex + 1, memory)

stateLast :: State -> State
stateLast (currentIndex, memory) = (currentIndex - 1, memory)

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

interpretLoop :: State -> AstList -> IO (State)
interpretLoop state instructions = do
  if (stateGetCurrent state) == 0 then
    return state
  else do
    steppedState <- interpret state instructions
    steppedState2 <- interpretLoop steppedState instructions
    return (steppedState2)

interpretStep :: State -> Ast -> IO (State)
interpretStep state x = do
  case x of
    Get -> do
      item <- unsafeCoerce (getChar)
      return (stateSetCurrent state item)
    Put -> do
      let c = stateGetCurrent state
      _ <- putChar (unsafeCoerce c) 
      return (state)
    Next -> do
      return (stateNext state)
    Last -> do
      return (stateLast state)
    While loopInstructions -> do
      transformedState <- interpretLoop state loopInstructions
      return (transformedState)
    Add ->
      return (stateSetCurrent state ((stateGetCurrent state) + 1))
    Sub ->
      return (stateSetCurrent state ((stateGetCurrent state) - 1))

interpret :: State -> AstList -> IO (State)
interpret state [] = do return (state)
interpret state (x:xs) = do
  updatedState <- interpretStep state x
  interpret updatedState xs

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

main = do
  args <- getArgs
  contents <- readFile (head args)
  putStrLn contents
  let parsed = parseProgram (trim contents)
  putStrLn (show parsed)
  interpret (0, empty) parsed
