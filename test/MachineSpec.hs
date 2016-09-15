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
        let automaton = Automaton { state = "A"
                                  , tapeBefore = ""
                                  , headSymbol = ' '
                                  , tapeAfter = ""
                                  }
        let transition = Transition { acceptState = "B"
                                    , acceptSymbol = emptySymbol
                                    , action = None
                                    , nextState = "A"
                                    }

        advance automaton [transition] `shouldBe` Nothing

    context "with exactly one matching transition" $ do
      it "should advance once then halt" $ do
        let automaton0 = Automaton { state = "State0"
                                   , tapeBefore = ""
                                   , headSymbol = 'A'
                                   , tapeAfter = ""
                                   }
        let transition = Transition { acceptState = "State0"
                                    , acceptSymbol = 'A'
                                    , action = None
                                    , nextState = "State1"
                                    }
        let automaton1 = automaton0 { state = "State1" }

        advance automaton0 [transition] `shouldBe` Just automaton1
        advance automaton1 [transition] `shouldBe` Nothing
