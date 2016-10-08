{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Machine             ( Automaton(..), Meta, Symbol, Transition
                                     , advance )
import           ConfigParser
import           ConfigMapper
import qualified Data.ByteString     as BS
import           Control.Arrow
import           Control.Monad
import           Data.Maybe
import           System.Console.ANSI
import           System.Environment  ( getArgs )
import           Control.Concurrent  ( threadDelay )
import           System.IO           ( hFlush, stdout )
import           Util                ( getOrFail, uncurry3 )

main :: IO ()
main = do
    delay : iterations : fileNames <- getArgs
    files <- sequence $ fmap BS.readFile fileNames
    let envs = fmap (getOrFail . getEnv) files
    runMachines envs (read iterations :: Int) (read delay :: Int)

data Env = Env { meta        :: Meta
               , automaton   :: Automaton
               , transitions :: [Transition]
               }

getEnv :: BS.ByteString -> Either String Env
getEnv str = uncurry3 Env <$> (readConfig str >>= mapConfig)
  where
    mapConfig :: MachineConfig -> Either String (Meta, Automaton, [Transition])
    mapConfig c = left show (fromConfig c)

runMachines :: [Env] -> Int -> Int -> IO ()
runMachines envs iterations delayMillis
    | iterations > 0 = do
          setCursorPosition 0 0
          clearFromCursorToScreenEnd

          forM_ envs (printTape . automaton)
          hFlush stdout
          threadDelay (delayMillis * 1000)

          runMachines (stepMachine <$> envs) (iterations - 1) delayMillis
    | otherwise = return ()

printTape :: Automaton -> IO ()
printTape a = do
    putStr $ tapeBefore a
    setSGR [ SetUnderlining SingleUnderline ]
    putChar $ headSymbol a
    setSGR [ SetUnderlining NoUnderline ]
    putStr $ tapeAfter a
    putChar '\n'

stepMachine :: Env -> Env
stepMachine e@(Env m a ts) =
    case next of
        Just a' -> e { automaton = a' }
        Nothing -> e
  where
    next = advance m a ts