module MachineSpec (spec) where

import           Machine
import           Prelude    hiding (Left)
import           Test.Hspec

{-# ANN module "HLint: ignore Redundant do" #-}


automaton0 :: Automaton
automaton0 = Automaton { state = "State0"
                       , tapeBefore = "AB"
                       , headSymbol = 'C'
                       , tapeAfter = "DE"
                       }

spec :: Spec
spec = do
  describe "Automaton" $ do
    context "with no transitions" $ do
      it "should halt" $ do
        advance automaton0 [] `shouldBe` Nothing

    context "with no matching accepting state but matching symbol" $ do
      it "should halt" $ do
        let transition = Transition { accept = ("State1", 'C')
                                    , action = None
                                    , nextState = "State0"
                                    }

        advance automaton0 [transition] `shouldBe` Nothing

    context "with no matching accepting symbol but matching state" $ do
      it "should halt" $ do
        let transition = Transition { accept = ("State0", 'X')
                                    , action = None
                                    , nextState = "State0"
                                    }

        advance automaton0 [transition] `shouldBe` Nothing

    context "with exactly one matching transition" $ do
      it "should advance once then halt" $ do
        let transition = Transition { accept = ("State0", 'C')
                                    , action = None
                                    , nextState = "State1"
                                    }

        let automaton1 = automaton0 { state = "State1" }
        advance automaton0 [transition] `shouldBe` Just automaton1
        advance automaton1 [transition] `shouldBe` Nothing

    context "with two different transitions" $ do
      it "should only pick matching transition" $ do
        let transition0 = Transition { accept = ("State0", 'C')
                                     , action = None
                                     , nextState = "State1"
                                     }
        let transition1 = transition0 { accept = ("State1", 'C'), nextState = "State2"}
        let badTransition = transition0 { accept = ("State1", 'X'), nextState = "State3"}
        let transitions = [transition0, transition1, badTransition]

        let automaton1 = automaton0 { state = "State1" }
        advance automaton0 transitions `shouldBe` Just automaton1
        let automaton2 = automaton1 { state = "State2" }
        advance automaton1 transitions `shouldBe` Just automaton2
        advance automaton2 transitions `shouldBe` Nothing

    it "should be able to move head left indefinitely" $ do
      let loop = Transition { accept = ("State0", '*')
                            , action = Left
                            , nextState = "State0"
                            }

      let automaton1 = automaton0 { tapeBefore = "A", headSymbol = 'B', tapeAfter = "CDE"}
      advance automaton0 [loop] `shouldBe` Just automaton1
      let automaton2 = automaton1 { tapeBefore = "", headSymbol = 'A', tapeAfter = "BCDE"}
      advance automaton1 [loop] `shouldBe` Just automaton2
      let automaton3 = automaton2 { tapeBefore = "", headSymbol = ' ', tapeAfter = "ABCDE"}
      advance automaton2 [loop] `shouldBe` Just automaton3
