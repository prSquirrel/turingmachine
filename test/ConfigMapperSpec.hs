module ConfigMapperSpec (spec) where

import           ConfigMapper
import           ConfigParser (MachineConfig (..), MetaConfig (..),
                               StartConfig (..))
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
          ConfigParser.meta = MetaConfig { ConfigParser.anySymbol = '_'
                                         , ConfigParser.emptySymbol = 'e'
                                         , ConfigParser.emptyTape = ""
                                         },
          start = StartConfig { ConfigParser.state = "SomeState0"
                              , tape = "ABCD[E]FGH"
                              }
        }
        let automaton = Automaton {
          Machine.meta = Meta { Machine.anySymbol = '_'
                              , Machine.emptySymbol = 'e'
                              , Machine.emptyTape = ""
                              },
          Machine.state = "SomeState0",
          tapeBefore = "ABCD",
          headSymbol = 'E',
          tapeAfter = "FGH"
        }

        fromConfig config `shouldBe` Right automaton
