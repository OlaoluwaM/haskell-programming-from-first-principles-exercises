module Playground where

import Data.Foldable (Foldable (foldl'))
import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)), DbNumber 9001, DbString "Hello, world!", DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDates []
 where
  getDates (DbDate date) acc = date : acc
  getDates _ acc = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr dBNumbers []
 where
  dBNumbers (DbNumber num) acc = num : acc
  dBNumbers _ acc = acc

mostRecentDate :: [DatabaseItem] -> UTCTime
mostRecentDate = maximum . filterDbDate

mostRecentDate' :: [DatabaseItem] -> UTCTime
mostRecentDate' = foldr getMostRecentDate (UTCTime (ModifiedJulianDay 0) 0)
 where
  getMostRecentDate (DbDate dt) = max dt
getMostRecentDate _ accDate = accDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

sumDb' :: [DatabaseItem] -> Integer
sumDb' = foldr sumDbNums 0
 where
  sumDbNums (DbNumber num) accNum = num + accNum
  sumDbNums _ accNum = accNum

avgDb :: [DatabaseItem] -> Double
avgDb dbIs = let dbNums = filterDbNumber dbIs in fromIntegral (fromInteger (sum dbNums) `div` length dbNums)

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

fibs' :: [Integer]
fibs' = take 20 $ fibs

fibs'' :: [Integer]
fibs'' = takeWhile (< 100) fibs

fibsN :: Int -> Integer
fibsN x = fibs !! x

factorials :: [Integer]
factorials = scanl (*) 1 [2 ..]

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

svs :: [(Char, Char, Char)]
svs = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

svs' :: [(Char, Char, Char)]
svs' = [(s1, v, s2) | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]

nouns :: [String]
nouns = ["Ola", "Andrew", "Dog", "Cat", "Monday", "Moon", "Sun", "Earth", "Laptop"]

verbs :: [String]
verbs = ["eat", "rehearse", "recite", "strike", "rise", "set", "speak", "promote", "take"]

nvn :: [(String, String, String)]
nvn = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

seekritFuncFractional :: String -> Double
seekritFuncFractional x = sum (map (fromIntegral . length) (words x)) / (fromIntegral . length) (words x)

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> f x || acc) False

myElem :: (Eq a) => a -> [a] -> Bool
myElem x = foldr (\x' _ -> x == x') False

myReverse :: [a] -> [a]
myReverse = foldl' (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr (\x acc -> if pred x then x : acc else acc) []

squish :: [[a]] -> [a]
squish = foldr (flip (++)) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\x acc -> acc ++ f x) []

squishMap' :: (a -> [b]) -> [a] -> [b]
squishMap' f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy compareFn (x : xs) = case compareFn x tailMax of
  GT -> x
  _ -> tailMax
 where
  tailMax = myMaximumBy compareFn xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy compareFn (x : xs) = case compareFn x tailMin of
  LT -> x
  _ -> tailMin
 where
  tailMin = myMinimumBy compareFn xs
