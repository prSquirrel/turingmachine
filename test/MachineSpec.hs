module MachineSpec (spec) where

import Test.Hspec
import Machine

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  describe "Automaton" $ do
    context "with no transitions" $ do
      it "should halt" $ do
        advance emptyAutomaton [] `shouldBe` Nothing

    context "with no accepting state" $ do
      it "should halt" $ do
        advance automaton [transition] `shouldBe` Nothing
          where automaton = emptyAutomaton { state = "A" }
                transition = Transition { acceptState = "B"
                                        , acceptSymbol = emptySymbol
                                        , action = None
                                        , nextState = "A"
                                        }
