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
import           System.Environment (getArgs)
import           Control.Concurrent (threadDelay)
import           System.IO (hFlush, stdout)
import           Util (uncurry3)

main :: IO ()
main = do
  [fileName, iterations, delay] <- getArgs
  configBody <- ByteString.readFile fileName
  let startEnv = getOrFail $ getEnv configBody
  uncurry3 runMachine startEnv (read iterations :: Int) (read delay :: Int)

getEnv :: ByteString -> Either String (Meta, Automaton, [Transition])
getEnv str = readConfig str >>= mapConfig
  where
    mapConfig c = left show (fromConfig c)

getOrFail :: Either String a -> a
getOrFail x =
  case x of
    Right a -> a
    Left e  -> error e

runMachine :: Meta -> Automaton -> [Transition] -> Int -> Int -> IO ()
runMachine m a ts iterations delayMillis = runMachine' m (Just a) ts iterations
  where
    halt = return ()
    runMachine' _ Nothing _ _ = halt
    runMachine' m' (Just a') ts' n'
      | n' > 0 = do
          printTape delayMillis a'
          let next = advance m' a' ts'
          runMachine' m' next ts' (n' - 1)
      | otherwise = halt

printTape :: Int -> Automaton -> IO ()
printTape delayMillis a = do
  setCursorColumn 0
  clearFromCursorToLineEnd

  putStr $ tapeBefore a
  setSGR [SetUnderlining SingleUnderline]
  putChar $ headSymbol a
  setSGR [SetUnderlining NoUnderline]
  putStr $ tapeAfter a

  hFlush stdout
  threadDelay (delayMillis * 1000)