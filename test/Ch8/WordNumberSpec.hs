module Ch8.WordNumberSpec where

import Ch8.Playground qualified as WordNumber
import Test.Hspec

spec :: Spec
spec = do
  describe "digitToWord" $ do
    it "returns zero for 0" $ do
      WordNumber.digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      WordNumber.digitToWord 1 `shouldBe` "one"

  describe "digits" $ do
    it "returns [1] for 1" $ do
      WordNumber.digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      WordNumber.digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber" $ do
    it "returns one-zero-zero gven 100" $ do
      WordNumber.wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      WordNumber.wordNumber 9001 `shouldBe` "nine-zero-zero-one"
