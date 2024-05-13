module Ch14.CipherSpec where

import Ch11.Cipher
import Ch9.Cipher

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Char

newtype AlphaChar = AlphaChar {unAlphaChar :: Char} deriving (Eq, Show)

genAlphaChar :: Gen AlphaChar
genAlphaChar = do
  let upperCaseAlphaRange = (ord 'A', ord 'Z')
  let lowerCaseAlphaRange = (ord 'a', ord 'z')
  charCode <- oneof [chooseInt upperCaseAlphaRange, chooseInt lowerCaseAlphaRange]

  pure $ AlphaChar $ chr charCode

instance Arbitrary AlphaChar where
  arbitrary = genAlphaChar

spec :: Spec
spec = do
  describe "Cipher Identity" $ do
    prop "Vigenere Cipher" $ \(NonEmpty keyword') (NonEmpty msg') ->
      let keyword = unAlphaChar <$> (keyword' :: [AlphaChar])
          msg = unAlphaChar <$> (msg' :: [AlphaChar])
       in vigenereUncipher keyword (vigenereCipher keyword msg) == msg

    prop "Caesar Cipher" $ \(Positive delta') (NonEmpty msg') ->
      let delta = (delta' :: Int)
          msg = unAlphaChar <$> (msg' :: [AlphaChar])
       in unCipherMsg delta (cipherMsg delta msg) == msg
