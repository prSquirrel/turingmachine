{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module ConfigParserSpec (spec) where

import           ConfigParser
import           Data.ByteString
import           Test.Hspec
import           Text.RawString.QQ


spec :: Spec
spec =
  describe "Config parser" $
    it "parses from YAML" $ do
      let str = [r|
      meta:
        emptyState: empty state
        anySymbol: _
        emptySymbol: e
        emptyTape: ""

      start:
        state: State0
        tape: A[B]C
      |]

      readConfig str `shouldBe` Right
        MachineConfig { meta = MetaConfig { emptyState = "empty state"
                                          , anySymbol = '_'
                                          , emptySymbol = 'e'
                                          , emptyTape = ""
                                          }
                      , machine = StartConfig { state = "State0"
                                              , tape = "A[B]C"
                                              }
                      }
