{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ConfigParserSpec (spec) where

import           ConfigParser
import           Data.ByteString
import           Machine (Meta(..))
import           Test.Hspec
import           Text.RawString.QQ

spec :: Spec
spec =
  describe "Parser" $
    context "with YAML supplied" $
      it "parses config" $ do
        let str = [r|
        meta:
          noActionSymbol: '*'
          anySymbol: _
          emptySymbol: e
          emptyTape: ""

        start:
          state: State0
          tape: A[B]C

        rules:
          - State0 C X N State1
          - State1 * Y R State2
        |]

        readConfig str `shouldBe` Right
                                    MachineConfig
                                      { meta = Meta
                                        { noActionSymbol = '*'
                                        , anySymbol = '_'
                                        , emptySymbol = 'e'
                                        , emptyTape = ""
                                        }
                                      , start = StartConfig { state = "State0", tape = "A[B]C" }
                                      , rules = ["State0 C X N State1", "State1 * Y R State2"]
                                      }