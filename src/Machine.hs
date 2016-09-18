module Machine where

import           Data.Generics.Aliases (orElse)
import           Data.Map.Strict       as Map hiding (map, null)
import           Prelude               hiding (Left, Right, lookup)
import           Safe                  (headDef, initDef, lastDef, tailDef)


data Automaton = Automaton { meta       :: Meta
                           , state      :: State
                           , tapeBefore :: [Symbol]
                           , headSymbol :: Symbol
                           , tapeAfter  :: [Symbol]
                           } deriving (Eq, Show)

data Meta = Meta { anySymbol   :: Symbol
                 , emptySymbol :: Symbol
                 , emptyTape   :: [Symbol]
                 } deriving (Eq, Show)

type State = String
type Symbol = Char


data Transition = Transition { accept    :: Accept
                             , action    :: Action
                             , nextState :: State
                             }

type Accept = (State, Symbol)
data Action = None | Move Direction | Write Symbol
data Direction = Left | Right




advance :: Automaton -> [Transition] -> Maybe Automaton
advance automaton transitions =
  let
    Automaton (Meta wildcard _ _) state _ current _ = automaton
    rules = buildAcceptanceMap transitions
    findForSymbol s = lookup (state, s) rules
    matchingTransition =
      findForSymbol current `orElse` findForSymbol wildcard
  in fmap (apply automaton) matchingTransition

buildAcceptanceMap :: [Transition] -> Map Accept Transition
buildAcceptanceMap transitions =
  Map.fromList tuples
    where tuples = map accept transitions `zip` transitions

apply :: Automaton -> Transition -> Automaton
apply automaton transition =
  (rollTape automaton action) { state = nextState }
    where Transition _ action nextState = transition

rollTape :: Automaton -> Action -> Automaton
rollTape automaton action =
  let
    Automaton (Meta anySymbol emptySymbol emptyTape) _ before current after = automaton
  in
    case action of
      None -> automaton
      Write s -> automaton { headSymbol = s }
      Move d -> case d of
        Left ->
          automaton { tapeBefore = initDef emptyTape before
                    , headSymbol = lastDef emptySymbol before
                    , tapeAfter = current : after
                    }
        Right ->
          automaton { tapeBefore = before ++ [current]
                    , headSymbol = headDef emptySymbol after
                    , tapeAfter = tailDef emptyTape after
                    }
