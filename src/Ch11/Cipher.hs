module Ch11.Cipher (vigenereCipher, CaesarCipher.cipherMsg, vigenereUncipher) where

import Ch9.Cipher qualified as CaesarCipher

import Data.Char

type Keyword = String
type Message = String
type CipherText = String

ceilingDivision :: Int -> Int -> Int
ceilingDivision num1 num2 = (ceiling @Double) . fromIntegral $ div num1 num2

standardizeKeywordLength :: Keyword -> Message -> String
standardizeKeywordLength keyword msg
  | length keyword < length msg = let replicateCount = ceilingDivision (length msg) (length keyword) in concat $ replicate (replicateCount + 1) keyword
  | otherwise = keyword

zipWithSpaces :: String -> String -> [(Char, Char)]
zipWithSpaces [] _ = []
zipWithSpaces _ [] = []
zipWithSpaces (' ' : as) listB = (' ', 'A') : zipWithSpaces as listB
zipWithSpaces (a : as) (b : bs) = (a, b) : zipWithSpaces as bs

getCharOffset :: Char -> Int
getCharOffset char
  | isLower char = ord char - ord 'a'
  | otherwise = ord char - ord 'A'

vigenereCipher :: Keyword -> Message -> String
vigenereCipher keyword msg = map (uncurry (flip CaesarCipher.caesarCipher) . fmap getCharOffset) . zipWithSpaces msg $ standardizeKeywordLength keyword msg

vigenereUncipher :: Keyword -> CipherText -> String
vigenereUncipher keyword msg = map (uncurry (flip CaesarCipher.unCaesarCipher) . fmap getCharOffset) . zipWithSpaces msg $ standardizeKeywordLength keyword msg

main :: IO ()
main = (print . vigenereUncipher "ALLY") (vigenereCipher "ALLY" "MEET AT DAWN")
