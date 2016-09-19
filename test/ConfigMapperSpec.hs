module ConfigMapperSpec (spec) where

import           ConfigMapper
import           ConfigParser (MachineConfig(..), Rules(..), StartConfig(..))
import           Data.Either
import           Machine
import           Prelude hiding (Right)
import           Test.Hspec

meta0 :: Meta
meta0 = Meta { anySymbol = '_', emptySymbol = 'e', emptyTape = "" }

spec :: Spec
spec =
  describe "Turing machine" $
    context "when supplied with a config" $
      it "initializes from it" $ do
        let config = MachineConfig
              { ConfigParser.meta = meta0
              , start = StartConfig { ConfigParser.state = "SomeState0", tape = "ABCD[E]FGH" }
              , rules = []
              }
        let automaton = Automaton
              { Machine.state = "SomeState0"
              , tapeBefore = "ABCD"
              , headSymbol = 'E'
              , tapeAfter = "FGH"
              }

        fromConfig config `shouldBe` Data.Either.Right (meta0, automaton, [])
