module ConfigMapper (fromConfig) where

import           ConfigParser
import           Data.Either
import           Machine
import           TapeParser
import           Text.Parsec

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

fromConfig :: MachineConfig -> Either ParseError (Meta, Automaton, [Transition])
fromConfig config =
  let MachineConfig meta startConfig rules = config
      StartConfig startState startTape = startConfig
      tapeless = Automaton startState
  in do
    automaton <- fmap (uncurry3 tapeless) (parseTape startTape)
    transitions <- fromRules meta rules
    return (meta, automaton, transitions)

fromRules :: Meta -> Rules -> Either ParseError [Transition]
fromRules _ [] = Data.Either.Right []