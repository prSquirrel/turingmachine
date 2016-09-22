module TokenizerSpec (spec) where

import           Tokenizer
import           Test.Hspec
import           Machine
import           Data.Either as Either

spec :: Spec
spec =
  describe "Tokenizer" $ do
    context "given machine tape" $
      it "returns (before, current, after)" $ do
        let tape = "ABCD[E]FGHJ "
        tokenizeTape tape `shouldBe` Either.Right ("ABCD", 'E', "FGHJ ")
    context "given rules" $ do
      let meta0 = Meta { noActionSymbol = '@', anySymbol = '_', emptySymbol = 'e', emptyTape = "" }
      context "which are empty" $
        it "returns no transitions" $
          tokenizeRules meta0 [] `shouldBe` Either.Right []
      context "with no write/move action" $
        it "returns transition with no actions" $
          tokenizeRules meta0 ["S0 A @ @ S1"] `shouldBe` Either.Right
                                                           [Transition ("S0", 'A') [] "S1"]
      context "with new symbol but no movement" $
        it "returns transition with write action" $
          tokenizeRules meta0 ["S0 C X @ S1"] `shouldBe` Either.Right
                                                           [Transition ("S0", 'C') [Write 'X'] "S1"]
      context "with write action equal to emptySymbol" $
        it "writes space" $
          tokenizeRules meta0 ["S0 C e @ S1"] `shouldBe` Either.Right
                                                           [Transition ("S0", 'C') [Write ' '] "S1"]
      context "given accept state equal to emptySymbol" $
        it "interprets it as space" $
          tokenizeRules meta0 ["S0 e @ @ S1"] `shouldBe` Either.Right
                                                           [Transition ("S0", ' ') [] "S1"]