module Machine where

import           Data.Generics.Aliases (orElse)
import           Data.Map.Strict       as Map hiding (map, null)
import           Prelude               hiding (Left, Right, lookup)


type State = String

emptyState :: State
emptyState = "0"

type Symbol = Char

anySymbol :: Symbol
anySymbol = '*'

emptySymbol :: Symbol
emptySymbol = ' '

emptyTape :: [Symbol]
emptyTape = ""

data Automaton = Automaton { state      :: State
                           , tapeBefore :: [Symbol]
                           , headSymbol :: Symbol
                           , tapeAfter  :: [Symbol]
                           } deriving (Eq, Show)

emptyAutomaton :: Automaton
emptyAutomaton = Automaton emptyState emptyTape emptySymbol emptyTape


data Action = None | Left | Right


type Accept = (State, Symbol)
data Transition = Transition { accept    :: Accept
                             , action    :: Action
                             , nextState :: State
                             }


advance :: Automaton -> [Transition] -> Maybe Automaton
advance _ [] = Nothing
advance automaton transitions =
  let
    rules = buildAcceptanceMap transitions
    findForSymbol s = lookup (state automaton, s) rules
    transition =
      findForSymbol (headSymbol automaton) `orElse` findForSymbol anySymbol
  in fmap (\t -> (rollTape automaton (action t)) { state = nextState t }) transition


buildAcceptanceMap :: [Transition] -> Map Accept Transition
buildAcceptanceMap transitions =
  Map.fromList tuples
    where tuples = map accept transitions `zip` transitions


rollTape :: Automaton -> Action -> Automaton
rollTape automaton action =
  let Automaton _ before current after = automaton
  in case action of
    None -> automaton
    Left ->
      automaton { tapeBefore = if null before then emptyTape else init before
                , headSymbol = if null before then emptySymbol else last before
                , tapeAfter = current : after
                }
    Right ->
      automaton { tapeBefore = before ++ [current]
                , headSymbol = if null after then emptySymbol else head after
                , tapeAfter = if null after then emptyTape else tail after
                }
