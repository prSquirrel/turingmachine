module TapeParserSpec (spec) where

import           TapeParser
import           Test.Hspec

spec :: Spec
spec =
  describe "Tape parser" $
    it "parses machine tape from string" $ do
      let tape = "ABCD[E]FGHJ "
      parseTape tape `shouldBe` Right ("ABCD", 'E', "FGHJ ")