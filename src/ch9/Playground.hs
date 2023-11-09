module Playground where

import Data.Char (isSpace)

eftInt :: Int -> Int -> [Int]
eftInt int1 int2
  | int1 > int2 = []
  | otherwise = go int1 int2 [int2]
 where
  go start stop enumerations
    | length enumerations == ((stop - start) + 1) = enumerations
    | otherwise = let count = length enumerations in go start stop ((stop - count) : enumerations)

eftChar :: Char -> Char -> [Char]
eftChar char1 char2
  | char1 > char2 = []
  | otherwise = go char1 char2 char2 [char2]
 where
  go startChar endChar currChar enumerations
    | startChar == currChar = enumerations
    | otherwise = let prevChar = pred currChar in go startChar endChar prevChar (prevChar : enumerations)

-- myWords "sheryl wants fun"
-- ["sheryl", "wants", "fun"]
-- using only takeWhile and dropWhile

myWords :: String -> [String]
myWords [] = []
myWords str
  | null word = []
  | otherwise = word : myWords rest
 where
  trim = dropWhile isSpace str
  word = takeWhile (not . isSpace) trim
  rest = dropWhile (not . isSpace) trim

-- myWords str =
--   if length word == 0
--     then []
--     else word : myWords rest
--  where
--   isSpace = flip elem " "
--   trim = dropWhile isSpace str
--   word = takeWhile (not . isSpace) trim
--   rest = dropWhile (not . isSpace) trim

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen =
  "Could frame thy fearful\
  \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual = ["Tyger Tyger, burning bright", "In the forests of the night", "What immortal hand or eye", "Could frame thy fearful symmetry?"]

myLines :: String -> [String]
myLines [] = []
myLines str
  | null word = []
  | otherwise = word : myLines rest
 where
  isNewline = flip elem "\n"
  trim = dropWhile isSpace str
  word = takeWhile (not . isNewline) trim
  rest = dropWhile (not . isNewline) trim

myChar :: Char -> String -> [String]
myChar _ [] = []
myChar charToBreakOn str
  | null word = []
  | otherwise = word : myChar charToBreakOn rest
 where
  isCharToBreakOn = flip elem [charToBreakOn]
  trim = dropWhile isSpace str
  word = takeWhile (not . isCharToBreakOn) trim
  rest = dropWhile (not . isCharToBreakOn) trim
