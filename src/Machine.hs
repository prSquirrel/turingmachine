module Machine where

import           Data.Map as Map hiding (null)
import           Prelude  hiding (Left, lookup)
--import qualified Data.Map.Strict as Map

-- a = Map.fromList [(1, 10), (2, 20)]
-- b = 1 `lookup` a
-- c = lookup 1 a

type State = String

emptyState :: State
emptyState = "0"

type Symbol = Char

anySymbol :: Symbol
anySymbol = '*'

emptySymbol :: Symbol
emptySymbol = ' '

data Automaton = Automaton { state      :: State
                           , tapeBefore :: [Symbol]
                           , headSymbol :: Symbol
                           , tapeAfter  :: [Symbol]
                           } deriving (Eq, Show)

emptyAutomaton :: Automaton
emptyAutomaton = Automaton emptyState "" emptySymbol ""

data Action = None | Left

type Accept = (State, Symbol)

data Transition = Transition { accept    :: Accept
                             , action    :: Action
                             , nextState :: State
                             }

--buildAcceptanceMap :: [Transition] -> Map (State, Symbol)

advance :: Automaton -> [Transition] -> Maybe Automaton
advance _ [] = Nothing
advance automaton transitions =
  if isAccepting (accept transition) (state automaton) (headSymbol automaton)
    then Just (rollTape automaton (action transition)) { state = nextState transition }
  else Nothing
    where transition = head transitions

isAccepting :: Accept -> State -> Symbol -> Bool
isAccepting (acceptState, acceptSymbol) state symbol =
  isAcceptingState acceptState state && isAcceptingSymbol acceptSymbol symbol

isAcceptingState accept state =
  accept == state

isAcceptingSymbol accept symbol =
  accept == anySymbol || symbol == accept

rollTape :: Automaton -> Action -> Automaton
rollTape automaton Left =
  automaton { tapeBefore = if null before then "" else init before
            , headSymbol = if null before then ' ' else last before
            , tapeAfter = current : after
            }
  where Automaton _ before current after = automaton

rollTape automaton _ = automaton
