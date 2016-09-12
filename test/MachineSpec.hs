module MachineSpec (spec) where

import Test.Hspec
import Machine

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Some func" $ do
    it "returns 0" $ do
      someFunc `shouldBe` 42
