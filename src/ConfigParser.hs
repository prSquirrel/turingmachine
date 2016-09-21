{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ConfigParser (MachineConfig(..), StartConfig(..), Rules, readConfig) where

import           Data.ByteString (ByteString)
import           Data.Yaml (FromJSON(..), (.:))
import qualified Data.Yaml as Yaml
import           Machine (Meta(..))

readConfig :: ByteString -> Either String MachineConfig
readConfig = Yaml.decodeEither

data MachineConfig = MachineConfig { meta :: Meta, start :: StartConfig, rules :: Rules }
  deriving (Eq, Show)

instance FromJSON MachineConfig where
  parseJSON (Yaml.Object o) = MachineConfig <$> o .: "meta" <*> o .: "start" <*> o .: "rules"

instance FromJSON Meta where
  parseJSON (Yaml.Object o) = Meta <$> o .: "noActionSymbol"
                                   <*> o .: "anySymbol"
                                   <*> o .: "emptySymbol"
                                   <*> o .: "emptyTape"

data StartConfig = StartConfig { state :: String, tape :: String }
  deriving (Eq, Show)

instance FromJSON StartConfig where
  parseJSON (Yaml.Object o) = StartConfig <$> o .: "state" <*> o .: "tape"

type Rules = [String]