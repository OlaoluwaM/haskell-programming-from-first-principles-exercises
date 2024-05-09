module Ch14.MorseSpec where

import Ch14.Morse
import Data.Map qualified as M
import Test.Hspec (Spec)
import Test.QuickCheck

import Test.Hspec.QuickCheck (prop)

allowedChars :: [Char]
allowedChars = M.keys letterToMorseMap

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorseMap

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_toMorseAndBackIdentity :: Property
prop_toMorseAndBackIdentity = forAll charGen $ \char -> (charToMorse char >>= morseToChar) == Just char

spec :: Spec
spec = prop "Tests for morse code module" prop_toMorseAndBackIdentity
