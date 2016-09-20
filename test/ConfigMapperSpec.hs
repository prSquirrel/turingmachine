module ConfigMapperSpec (spec) where

import           ConfigMapper
import           ConfigParser (MachineConfig(..), Rules(..), StartConfig(..))
import           Data.Either
import           Machine
import           Prelude hiding (Right)
import           Test.Hspec

meta0 = Meta { anySymbol = '_', emptySymbol = 'e', emptyTape = "" }

configForRules rules = MachineConfig
  { ConfigParser.meta = meta0
  , start = StartConfig { ConfigParser.state = "SomeState0", tape = "ABCD[E]FGH" }
  , rules = []
  }

automaton = Automaton
  { Machine.state = "SomeState0"
  , tapeBefore = "ABCD"
  , headSymbol = 'E'
  , tapeAfter = "FGH"
  }

spec :: Spec
spec =
  describe "Turing machine" $
    context "when supplied with a config" $ do
      context "with empty rule list" $
        it "initializes having no transitions" $
          fromConfig (configForRules []) `shouldBe` Data.Either.Right (meta0, automaton, [])