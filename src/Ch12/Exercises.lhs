import Data.Char (isUpperCase)
Chapter 12 Exercises
==============================

Determine the kinds
=====================

1. Q: Given `id :: a -> a` What is the kind of `a` | A: `a` is of kind `Type`/`*`

2 Q: Given `r :: a -> f a` What are the kinds of `a` and `f` | A: `a` is of kind `Type`/`*` while `f` is of kind `Type -> Type`

String processing
=====================

1. Write a recursive function named replaceThe that takes a text/string, breaks it into words, and replaces each instance of "the" with "a". It should only replace exactly the word "the". `notThe` is a suggested helper function for accomplishing this:

$$
\begin{code}
import Data.Char

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
    Nothing -> if isTitleCase str then "A" : go rest else "a" :  go rest

\end{code}
$$

2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of "the" followed by a vowel-initial word:

$$
\begin{code}
startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel str = let vowels = "aeiou" in head str `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go False 0 . words
 where
  go _ count [] = count
  go False count (str : rest) = go (str == "the" || str == "The") count rest
  go True count (str : rest) = let newCount = if startsWithVowel str then count + 1 else count in go False newCount rest
\end{code}
$$

Validate the word
=====================

$$
\begin{code}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

vowels :: String
vowels = "aeiou"

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
\end{code}
$$

Validate the word
=====================

$$
\begin{code}
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = natToInteger nat + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n > 0 = Just Succ <*> integerToNat (n - 1)
  | otherwise = Nothing
\end{code}
$$
