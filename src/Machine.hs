module Machine where

import Prelude hiding (lookup)
import Data.Map as Map hiding (null)
--import qualified Data.Map.Strict as Map

-- a = Map.fromList [(1, 10), (2, 20)]
-- b = 1 `lookup` a
-- c = lookup 1 a

type State = String

emptyState :: State
emptyState = "0"

type Symbol = Char

emptySymbol :: Symbol
emptySymbol = ' '

data Automaton = Automaton { state :: State
                           , tapeBefore :: [Symbol]
                           , headSymbol :: Symbol
                           , tapeAfter :: [Symbol]
                           } deriving (Eq, Show)

emptyAutomaton :: Automaton
emptyAutomaton = Automaton emptyState "" emptySymbol ""

data Action = Left | Right | None | Write Symbol

data Transition = Transition { acceptState :: State
                             , acceptSymbol :: Symbol
                             , action :: Action
                             , nextState :: State
                             }

--buildAcceptanceMap :: [Transition] -> Map (State, Symbol)

advance :: Automaton -> [Transition] -> Maybe Automaton
advance automaton transitions =
  if null transitions || state automaton /= acceptState transition
    then Nothing
  else Just automaton { state = nextState transition }
    where transition = head transitions
