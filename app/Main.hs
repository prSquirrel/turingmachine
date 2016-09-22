{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Machine (Meta, Automaton(..), Transition, Symbol, advance)
import           ConfigParser
import           ConfigMapper
import           Data.ByteString as ByteString hiding (putStr)
import           Control.Arrow
import           Control.Monad
import           Data.Maybe
import           System.Console.ANSI
import System.Environment (getArgs)

main :: IO ()
main = do
  [fileName, iterations] <- getArgs
  body <- ByteString.readFile fileName
  let (meta, automaton, transitions) =
        case getEnv body of
          Right a -> a
          Left e  -> error e
  let results = runMachine meta (Just automaton) transitions (read iterations :: Int) []
  forM_ results printTape

printTape :: Automaton -> IO ()
printTape a = do
    putStr $ tapeBefore a
    setSGR [SetUnderlining SingleUnderline]
    putChar $ headSymbol a
    setSGR [SetUnderlining NoUnderline]
    putStr $ tapeAfter a
    putStr $ "\n\n"

getEnv :: ByteString -> Either String (Meta, Automaton, [Transition])
getEnv str = readConfig str >>= mapConfig
  where
    mapConfig c = left show (fromConfig c)

runMachine :: Meta -> Maybe Automaton -> [Transition] -> Int -> [Automaton] -> [Automaton]
runMachine _ Nothing _ _ xs = Prelude.reverse xs
runMachine m (Just a) ts iterations xs
  | iterations > 0 =
      let next = advance m a ts
      in runMachine m next ts (iterations - 1) (a : xs)
  | otherwise = Prelude.reverse xs