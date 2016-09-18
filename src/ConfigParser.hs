{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigParser where

import           Data.ByteString (ByteString)
import           Data.Yaml       (FromJSON (..), (.:))
import qualified Data.Yaml       as Yaml
import           Machine         (Meta (..))

--TODO: map config directly, without using intermediate types

readRules :: ByteString -> Either String Rules
readRules = Yaml.decodeEither

data Rules = Rules [String] deriving (Eq, Show)

instance FromJSON Rules where
  parseJSON (Yaml.Object o) = Rules <$>
    o .: "rules"

readConfig :: ByteString -> Either String MachineConfig
readConfig = Yaml.decodeEither

data MachineConfig = MachineConfig { meta  :: Meta
                                   , start :: StartConfig
                                   } deriving (Eq, Show)

instance FromJSON MachineConfig where
  parseJSON (Yaml.Object o) = MachineConfig <$>
    o .: "meta" <*>
    o .: "start"

instance FromJSON Meta where
  parseJSON (Yaml.Object o) = Meta <$>
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
