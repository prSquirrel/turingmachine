module MachineSpec (spec) where

import           Machine
import           Prelude    hiding (Left)
import           Test.Hspec

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
                                  , headSymbol = 'X'
                                  , tapeAfter = ""
                                  }
        let transition = Transition { accept = ("B", 'X')
                                    , action = None
                                    , nextState = "A"
                                    }

        advance automaton [transition] `shouldBe` Nothing

    context "with no accepting symbol" $ do
      it "should halt" $ do
        let automaton = Automaton { state = "A"
                                  , tapeBefore = ""
                                  , headSymbol = 'X'
                                  , tapeAfter = ""
                                  }
        let transition = Transition { accept = ("A", 'Y')
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
        let transition = Transition { accept = ("State0", 'A')
                                    , action = None
                                    , nextState = "State1"
                                    }
        let automaton1 = automaton0 { state = "State1" }

        advance automaton0 [transition] `shouldBe` Just automaton1
        advance automaton1 [transition] `shouldBe` Nothing

    it "should be able to move head left indefinitely" $ do
      let automaton0 = Automaton { state = "LoopState"
                                 , tapeBefore = "AB"
                                 , headSymbol = 'C'
                                 , tapeAfter = "DE"
                                 }
      let automaton1 = automaton0 { tapeBefore = "A", headSymbol = 'B', tapeAfter = "CDE"}
      let automaton2 = automaton0 { tapeBefore = "", headSymbol = 'A', tapeAfter = "BCDE"}
      let automaton3 = automaton0 { tapeBefore = "", headSymbol = ' ', tapeAfter = "ABCDE"}

      let loop = Transition { accept = ("LoopState", '*')
                            , action = Left
                            , nextState = "LoopState"
                            }

      advance automaton0 [loop] `shouldBe` Just automaton1
      advance automaton1 [loop] `shouldBe` Just automaton2
      advance automaton2 [loop] `shouldBe` Just automaton3
