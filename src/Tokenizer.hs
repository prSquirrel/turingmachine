module Tokenizer (tokenizeTape, tokenizeRules) where

import           Control.Applicative ((*>), (<*))
import           ConfigParser (Rules)
import           Machine
import           Data.Either as Either
import           Text.Parsec hiding (State)
import           Text.Parsec.String
import           Control.Monad (sequence)

tokenizeTape :: String -> Either ParseError ([Symbol], Symbol, [Symbol])
tokenizeTape = parse tapeParser "tape parser"

tapeParser :: Parser ([Symbol], Symbol, [Symbol])
tapeParser = do
  before <- many (noneOf "[")
  current <- char '[' *> anyChar <* char ']'
  after <- many anyChar
  return (before, current, after)

tokenizeRules :: Meta -> Rules -> Either ParseError [Transition]
tokenizeRules _ [] = Either.Right []
tokenizeRules m rules =
  let parsedRules = parse (ruleParser m) "rule parser" `map` rules
      transformedRules = fmap (ruleToTransition m) `map` parsedRules
  in sequence transformedRules

ruleParser :: Meta -> Parser (State, Symbol, Symbol, Symbol, State)
ruleParser m = do
  state <- word
  acceptSymbol <- space *> anyChar
  writeSymbol <- space *> anyChar
  moveSymbol <- space *> (char (noActionSymbol m) <|> (char 'L' <|> char 'R'))
  nextState <- space *> word
  return (state, acceptSymbol, writeSymbol, moveSymbol, nextState)

word = many1 (noneOf " ")

ruleToTransition :: Meta -> (State, Symbol, Symbol, Symbol, State) -> Transition
ruleToTransition m (acceptState, acceptSymbol, writeSymbol, moveSymbol, nextState) =
  Transition (acceptState, checkForSpace acceptSymbol) (write ++ move) nextState
  where
    write
      | noActionSymbol m == writeSymbol = []
      | emptySymbol m == writeSymbol = [Write ' ']
      | otherwise = [Write writeSymbol]
    move
      | noActionSymbol m == moveSymbol = []
      | otherwise = [Move (toDirection moveSymbol)]
    toDirection 'L' = Machine.Left
    toDirection 'R' = Machine.Right
    checkForSpace c = if emptySymbol m == c then ' ' else c