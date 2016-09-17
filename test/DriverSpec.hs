module DriverSpec (spec) where

import           Driver
import           Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}


spec :: Spec
spec = do
  describe "Driver" $ do
    it "parses machine tape from string" $ do
      let config = "ABCD[E]FGHJ "
      parseTape config `shouldBe` ("ABCD", 'E', "FGHJ ")
