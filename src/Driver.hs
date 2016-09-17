{-# LANGUAGE FlexibleContexts #-}

module Driver where

import           Control.Applicative ((*>), (<*))
import           Machine             (Symbol)
import           Text.Parsec
import           Text.Parsec.String


tapeParser :: Parser ([Symbol], Symbol, [Symbol])
tapeParser = do
  before <- many (noneOf "[")
  current <- char '[' *> anyChar <* char ']'
  after <- many anyChar
  return (before, current, after)

parseTape :: String -> ([Symbol], Symbol, [Symbol])
parseTape str =
  tape
    where tape = case parse tapeParser "tape parser" str of Right a -> a
