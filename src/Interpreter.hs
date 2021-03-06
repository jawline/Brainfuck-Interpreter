module Interpreter where
import Unsafe.Coerce (unsafeCoerce)
import State
import Ast

interpretLoop :: State -> AstList -> IO (State)
interpretLoop state instructions = do
  case stateGetCurrent state of
    0 -> return state
    _ -> do
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
    Next -> return (stateNext state)
    Last -> return (stateLast state)
    While loopInstructions -> do
      transformedState <- interpretLoop state loopInstructions
      return (transformedState)
    Add -> return (stateSetCurrent state ((stateGetCurrent state) + 1))
    Sub -> return (stateSetCurrent state ((stateGetCurrent state) - 1))

interpret :: State -> AstList -> IO (State)
interpret state [] = do return (state)
interpret state (x:xs) = do
  updatedState <- interpretStep state x
  interpret updatedState xs
