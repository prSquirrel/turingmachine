module ConfigMapper where


import           ConfigParser
import           Machine      (Automaton (..), Meta (..))
import           TapeParser
import           Text.Parsec


uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

fromConfig :: MachineConfig -> Either ParseError Automaton
fromConfig config =
  let
    MachineConfig metaConfig startConfig = config
    MetaConfig wildcard emptySymbol emptyTape = metaConfig
    StartConfig startState startTape = startConfig
    tapeless = Automaton (Meta wildcard emptySymbol emptyTape) startState
  in
    fmap (uncurry3 tapeless) (parseTape startTape)
