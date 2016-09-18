module ConfigMapperSpec (spec) where

import           ConfigMapper
import           ConfigParser (MachineConfig (..), StartConfig (..))
import           Data.Either
import           Machine      (Automaton (..), Meta (..))
import           Prelude      hiding (Right)
import           Test.Hspec


spec :: Spec
spec =
  describe "Turing machine" $
    context "when supplied a config" $
      it "initializes from it" $ do
        let config = MachineConfig {
          ConfigParser.meta = Meta { anySymbol = '_'
                                   , emptySymbol = 'e'
                                   , emptyTape = ""
                                   },
          start = StartConfig { ConfigParser.state = "SomeState0"
                              , tape = "ABCD[E]FGH"
                              }
        }
        let automaton = Automaton {
          Machine.meta = Meta { anySymbol = '_'
                              , emptySymbol = 'e'
                              , emptyTape = ""
                              },
          Machine.state = "SomeState0",
          tapeBefore = "ABCD",
          headSymbol = 'E',
          tapeAfter = "FGH"
        }

        fromConfig config `shouldBe` Right automaton
