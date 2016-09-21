module ConfigMapperSpec (spec) where

import           ConfigMapper
import           ConfigParser (MachineConfig(..), Rules(..), StartConfig(..))
import           Data.Either as Either
import           Machine
import           Prelude hiding (Right)
import           Test.Hspec

meta0 = Meta { noActionSymbol = '*', anySymbol = '_', emptySymbol = 'e', emptyTape = "" }

configForRules rules = MachineConfig
  { ConfigParser.meta = meta0
  , start = StartConfig { ConfigParser.state = "SomeState0", tape = "ABCD[E]FGH" }
  , rules = rules
  }

automaton = Automaton
  { Machine.state = "SomeState0"
  , tapeBefore = "ABCD"
  , headSymbol = 'E'
  , tapeAfter = "FGH"
  }

transitions = [ Transition
                { accept = ("State0", 'A')
                , actions = [Write 'X', Move Machine.Right]
                , nextState = "State1"
                }
              , Transition { accept = ("State1", 'B'), actions = [Write 'Y'], nextState = "State2" }
              , Transition
                { accept = ("State2", 'C')
                , actions = [Move Machine.Left]
                , nextState = "State3"
                }
              , Transition { accept = ("State3", 'D'), actions = [], nextState = "State4" }
              ]

spec :: Spec
spec =
  describe "Turing machine" $
    context "when supplied with a config" $ do
      context "with empty rule list" $
        it "initializes having no transitions" $
          fromConfig (configForRules []) `shouldBe` Either.Right (meta0, automaton, [])
      it "builds intermediate actions for transitions" $
        fromConfig
          (configForRules
             [ "State0 A X R State1"
             , "State1 B Y * State2"
             , "State2 C * L State3"
             , "State3 D * * State4"
             ]) `shouldBe` Either.Right (meta0, automaton, transitions)