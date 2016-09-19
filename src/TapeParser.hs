module TapeParser (parseTape) where

import           Control.Applicative ((*>), (<*))
import           Machine (Symbol)
import           Text.Parsec
import           Text.Parsec.String

parseTape :: String -> Either ParseError ([Symbol], Symbol, [Symbol])
parseTape = parse tapeParser "tape parser"

tapeParser :: Parser ([Symbol], Symbol, [Symbol])
tapeParser = do
  before <- many (noneOf "[")
  current <- char '[' *> anyChar <* char ']'
  after <- many anyChar
  return (before, current, after)