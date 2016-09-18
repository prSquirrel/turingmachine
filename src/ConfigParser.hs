{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigParser where

import           Data.ByteString (ByteString)
import           Data.Yaml       (FromJSON (..), (.:))
import qualified Data.Yaml       as Yaml


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
