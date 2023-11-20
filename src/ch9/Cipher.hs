module Cipher where

import Data.Char

upperAlphabetSize :: Int
upperAlphabetSize = ord 'Z' - ord 'A'

wrapUpperAlphabetPosition :: Int -> Int
wrapUpperAlphabetPosition pos = (pos `mod` (upperAlphabetSize + 1)) + ord 'A'

toBaseUpper :: Int -> Int
toBaseUpper = subtract (ord 'A')

lowerAlphabetSize :: Int
lowerAlphabetSize = ord 'z' - ord 'a'

wrapLowerAlphabetPosition :: Int -> Int
wrapLowerAlphabetPosition pos = (pos `mod` (lowerAlphabetSize + 1)) + ord 'a'

toBaseLower :: Int -> Int
toBaseLower = subtract (ord 'a')

caesarCipher :: Int -> Char -> Char
caesarCipher delta char
  | isLower char = chr $ wrapLowerAlphabetPosition $ (+ delta) $ toBaseLower $ ord char
  | isUpper char = chr $ wrapUpperAlphabetPosition $ (+ delta) $ toBaseUpper $ ord char
  | otherwise = char

unCaesarCipher :: Int -> Char -> Char
unCaesarCipher reverseDelta char
  | isLower char = chr $ wrapLowerAlphabetPosition $ subtract reverseDelta $ toBaseLower $ ord char
  | isUpper char = chr $ wrapUpperAlphabetPosition $ subtract reverseDelta $ toBaseUpper $ ord char
  | otherwise = char

cipherMsg :: Int -> String -> String
cipherMsg delta = unwords . map (map $ caesarCipher delta) . words

unCipherMsg :: Int -> String -> String
unCipherMsg reverseDelta = unwords . map (map $ unCaesarCipher reverseDelta) . words
