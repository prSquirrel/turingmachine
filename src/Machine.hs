module Machine where

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

advance :: Automaton -> [Transition] -> Maybe Automaton
advance automaton transitionTable = Nothing
