module State where
import Data.Word (Word8)
import Data.Map (Map, findWithDefault, insert, empty)

type State = (Integer, Map Integer Word8)

initialState :: State
initialState = (0, empty)

stateGetCurrent :: State -> Word8
stateGetCurrent (currentIndex, memory) = findWithDefault 0 currentIndex memory 

stateSetCurrent :: State -> Word8 -> State
stateSetCurrent (currentIndex, memory) val = (currentIndex, insert currentIndex val memory)

stateNext :: State -> State
stateNext (currentIndex, memory) = (currentIndex + 1, memory)

stateLast :: State -> State
stateLast (currentIndex, memory) = (currentIndex - 1, memory)
