{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ch12.Playground where

import Data.Char (isUpperCase)

import Data.Bool (bool)

vowels :: String
vowels = "aeiou"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe "The" = Nothing
notThe str = Just str

isTitleCase :: String -> Bool
isTitleCase "" = False
isTitleCase str = isUpperCase $ head str

replaceThe :: String -> String
replaceThe = unwords . go . words
 where
  go [] = []
  go ("the" : rest) = "a" : go rest
  go ("The" : rest) = "A" : go rest
  go (str : rest) = str : go rest

replaceThe' :: String -> String
replaceThe' = unwords . go . words
 where
  go [] = []
  go (str : rest) = case notThe str of
    Just notTheStr -> notTheStr : go rest
    Nothing -> if isTitleCase str then "A" : go rest else "a" : go rest

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel str = head str `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go False 0 . words
 where
  go _ count [] = count
  go False count (str : rest) = go (str == "the" || str == "The") count rest
  go True count (str : rest) = let newCount = if startsWithVowel str then count + 1 else count in go False newCount rest

newtype Word' = Word' String deriving (Eq, Show)

newtype VowelCount = VowelCount Integer deriving (Eq, Show, Num)
newtype ConsonantCount = ConsonantCount Integer deriving (Eq, Show, Num)

vowelCountToInteger :: VowelCount -> Integer
vowelCountToInteger (VowelCount n) = n

consonantCountToInteger :: ConsonantCount -> Integer
consonantCountToInteger (ConsonantCount n) = n

isVowel :: Char -> Bool
isVowel = flip elem vowels

mkWord :: String -> Maybe Word'
mkWord word = bool (Just (Word' word)) Nothing $ areVowelsMore $ getVowelAndConsonantCount word
 where
  getVowelAndConsonantCount :: String -> (VowelCount, ConsonantCount)
  getVowelAndConsonantCount = foldr (\char (vowelCount, consonantCount) -> if isVowel char then (vowelCount + 1, consonantCount) else (vowelCount, consonantCount + 1)) (0, 0)

  areVowelsMore (vowelCount, consonantCount) = vowelCountToInteger vowelCount > consonantCountToInteger consonantCount

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = natToInteger nat + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n > 0 = Just Succ <*> integerToNat (n - 1)
  | otherwise = Nothing
