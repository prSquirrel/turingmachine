module Machine (
    Meta(..),
    Automaton(..),
    Transition(..),
    Symbol,
    State,
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

data Meta =
       Meta
         { noActionSymbol :: Symbol
         , anySymbol :: Symbol
         , emptySymbol :: Symbol
         , emptyTape :: [Symbol]
         }
  deriving (Eq, Show)

type State = String

type Symbol = Char

data Transition = Transition { accept :: Accept, actions :: [Action], nextState :: State }
  deriving (Eq, Show)

type Accept = (State, Symbol)

data Action = Move Direction
            | Write Symbol
  deriving (Eq, Show)

data Direction = Left
               | Right
  deriving (Eq, Show)

advance :: Meta -> Automaton -> [Transition] -> Maybe Automaton
advance meta automaton transitions =
  let Meta _ wildcard _ _ = meta
      Automaton state _ current _ =
                                     automaton
      rules = buildAcceptanceMap transitions
      findForSymbol s = lookup (state, s) rules
      matchingTransition = findForSymbol current `orElse`
                           findForSymbol wildcard
  in fmap (applyTransition meta automaton) matchingTransition

buildAcceptanceMap :: [Transition] -> Map Accept Transition
buildAcceptanceMap transitions =
  Map.fromList tuples
  where
    tuples = map accept transitions `zip` transitions

applyTransition :: Meta -> Automaton -> Transition -> Automaton
applyTransition meta automaton (Transition _ actions nextState) =
  (applyActions meta automaton actions) { state = nextState }

applyActions :: Meta -> Automaton -> [Action] -> Automaton
applyActions _ automaton [] = automaton
applyActions m automaton (x:xs) =
  let Meta _ anySymbol emptySymbol emptyTape = m
      Automaton _ before current after = automaton
      newAutomaton =
                      case x of
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
  in applyActions m newAutomaton xs