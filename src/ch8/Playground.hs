module Playground where

import Data.List (intercalate, intersperse)
import GHC.Num (integerIsNegative)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"

recursiveSum :: (Eq a, Num a) => a -> a
recursiveSum n = go n n
 where
  go 0 summation = summation
  go n summation = let predecessor = n - 1 in go predecessor (summation + predecessor)

recursiveMult :: (Integral a) => a -> a -> a
recursiveMult _ 0 = 0
recursiveMult n multiplier = go n multiplier 0
 where
  go _ 0 output = output
  go n multiplier output = go n (multiplier - 1) (output + n)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

dividedBy :: Numerator -> Denominator -> Maybe (Quotient, Remainder)
dividedBy n 0 = Nothing
dividedBy n d
  | integerIsNegative n && integerIsNegative d = Just $ go (abs n) (abs d) 0
  | integerIsNegative n || integerIsNegative d = Just $ fstMap (* (-1)) (go (abs n) (abs d) 0)
  | otherwise = Just $ go n d 0
 where
  go :: Numerator -> Denominator -> Integer -> (Quotient, Remainder)
  go n' 1 _ = (n', 0)
  go n' d' c = if n' < d' || n' == 0 then (c, n') else go (n' - d') d' (c + 1)

fstMap :: (Num a) => (a -> a) -> (a, a) -> (a, a)
fstMap f (x, y) = (f x, y)

mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 (n + 11)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "not a single digit" -- For any other input

digits :: Int -> [Int]
digits n = go n []
 where
  go :: Int -> [Int] -> [Int]
  go n' digits'
    | n' < 10 = n' : digits'
    | otherwise = go (div n' 10) (mod n' 10 : digits')

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits
