module ConfigMapper (fromConfig) where

import           ConfigParser
import           Data.Either
import           Machine
import           Tokenizer
import           Text.Parsec
import           Util (uncurry3)

fromConfig :: MachineConfig -> Either ParseError (Meta, Automaton, [Transition])
fromConfig config =
  let MachineConfig meta startConfig rules = config
      StartConfig startState startTape = startConfig
      tapeless = Automaton startState
  in do
    automaton <- fmap (uncurry3 tapeless) (tokenizeTape startTape)
    transitions <- tokenizeRules meta rules
    return (meta, automaton, transitions)