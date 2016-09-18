{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}


module ConfigParserSpec (spec) where

import           ConfigParser
import           Data.ByteString
import           Machine           (Meta (..))
import           Test.Hspec
import           Text.RawString.QQ


spec :: Spec
spec =
  describe "Config parser" $
    it "parses from YAML" $ do
      let str = [r|
      meta:
        anySymbol: _
        emptySymbol: e
        emptyTape: ""

      start:
        state: State0
        tape: A[B]C
      |]

      readConfig str `shouldBe` Right
        MachineConfig { meta = Meta { anySymbol = '_'
                                    , emptySymbol = 'e'
                                    , emptyTape = ""
                                    }
                      , start = StartConfig { state = "State0"
                                            , tape = "A[B]C"
                                            }
                      }