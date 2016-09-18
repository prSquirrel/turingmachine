{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigParser where

import           Control.Applicative ((*>), (<$>), (<*), (<*>))
import           Data.ByteString     (ByteString)
import           Data.Yaml           (FromJSON (..), (.:))
import qualified Data.Yaml           as Yaml
import           Machine             (Symbol)
import           Text.Parsec
import           Text.Parsec.String


parseTape :: String -> Either ParseError ([Symbol], Symbol, [Symbol])
parseTape =
  parse tapeParser "tape parser"

tapeParser :: Parser ([Symbol], Symbol, [Symbol])
tapeParser = do
  before <- many (noneOf "[")
  current <- char '[' *> anyChar <* char ']'
  after <- many anyChar
  return (before, current, after)


readConfig :: ByteString -> Either String MachineConfig
readConfig = Yaml.decodeEither

data MachineConfig = MachineConfig { meta    :: MetaConfig
                                   , machine :: StartConfig
                                   } deriving (Eq, Show)

instance FromJSON MachineConfig where
  parseJSON (Yaml.Object o) = MachineConfig <$>
    o .: "meta" <*>
    o .: "start"


data MetaConfig = MetaConfig { emptyState  :: String
                             , anySymbol   :: Char
                             , emptySymbol :: Char
                             , emptyTape   :: String
                             } deriving (Eq, Show)

instance FromJSON MetaConfig where
  parseJSON (Yaml.Object o) = MetaConfig <$>
    o .: "emptyState" <*>
    o .: "anySymbol" <*>
    o .: "emptySymbol" <*>
    o .: "emptyTape"


data StartConfig = StartConfig { state :: String
                               , tape  :: String
                               } deriving (Eq, Show)

instance FromJSON StartConfig where
  parseJSON (Yaml.Object o) = StartConfig <$>
    o .: "state" <*>
    o .: "tape"
