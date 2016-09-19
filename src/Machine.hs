module Machine (
    Meta(..),
    Automaton(..),
    Transition(..),
    Symbol,
    advance,
    Direction(..),
    Action(..),
    ) where

import           Data.Generics.Aliases (orElse)
import           Data.Map.Strict as Map hiding (map, null)
import           Prelude hiding (Left, Right, lookup)
import           Safe (headDef, initDef, lastDef, tailDef)

data Automaton =
       Automaton
         { state :: State
         , tapeBefore :: [Symbol]
         , headSymbol :: Symbol
         , tapeAfter :: [Symbol]
         }
  deriving (Eq, Show)

data Meta = Meta { anySymbol :: Symbol, emptySymbol :: Symbol, emptyTape :: [Symbol] }
  deriving (Eq, Show)

type State = String

type Symbol = Char

data Transition = Transition { accept :: Accept, actions :: [Action], nextState :: State }
  deriving (Eq, Show)

type Accept = (State, Symbol)

data Action = None
            | Move Direction
            | Write Symbol
  deriving (Eq, Show)

data Direction = Left
               | Right
  deriving (Eq, Show)

advance :: Meta -> Automaton -> [Transition] -> Maybe Automaton
advance meta automaton transitions =
  let Meta wildcard _ _ = meta
      Automaton state _ current _ =
                                     automaton
      rules = buildAcceptanceMap transitions
      findForSymbol s = lookup (state, s) rules
      matchingTransition = findForSymbol current `orElse`
                           findForSymbol wildcard
  in fmap (apply meta automaton) matchingTransition

buildAcceptanceMap :: [Transition] -> Map Accept Transition
buildAcceptanceMap transitions =
  Map.fromList tuples
  where
    tuples = map accept transitions `zip` transitions

apply :: Meta -> Automaton -> Transition -> Automaton
apply meta automaton (Transition _ actions nextState) =
  (rollTape meta automaton (head actions)) { state = nextState }

rollTape :: Meta -> Automaton -> Action -> Automaton
rollTape meta automaton action =
  let Meta anySymbol emptySymbol emptyTape =
                                              meta
      Automaton _ before current after =
                                          automaton
  in case action of
    None -> automaton
    Write s -> automaton { headSymbol = s }
    Move d ->
      case d of
        Left -> automaton
          { tapeBefore = initDef emptyTape before
          , headSymbol = lastDef emptySymbol before
          , tapeAfter = current : after
          }
        Right -> automaton
          { tapeBefore = before ++ [current]
          , headSymbol = headDef emptySymbol after
          , tapeAfter = tailDef emptyTape after
          }