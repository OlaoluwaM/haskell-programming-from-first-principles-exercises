{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
module Exercises where

-- Case Practice

-- 1.
fucntionC :: (Num x, Ord x) => x -> x -> x
fucntionC x y = case x > y of
  True -> x
  False -> y

-- 2.
ifEvenAdd2 :: (Integral a) => a -> a
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3.
nums :: (Ord a, Num a) => a -> a
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  EQ -> 0

-- Artful Dodgy
dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = flip dodgy 2

-- Guard Duty

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | grade >= 0.7 = 'C'
  | grade >= 0.9 = 'A'
  | grade < 0.59 = 'F'
  | grade >= 0.59 = 'D'
  | grade >= 0.8 = 'B'
  | otherwise = 'F'
 where
  grade = x / 100

pal :: (Eq a) => [a] -> Bool -- Returns b
pal xs
  | xs == reverse xs = True
  | otherwise = False

numbers :: (Num a, Ord a) => a -> a -- Returns c
numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1

-- Chapter Exercises Code portion

tensDigit :: (Integral a) => a -> a
tensDigit = snd . (`divMod` 10) . fst . (`divMod` 10)

tensDigit' :: (Integral a) => a -> a
tensDigit' x = d
 where
  xLast = x `div` 10
  d = xLast `mod` 10

-- b. Yes the `divMod` version has the same type signature as the `div` & `mod` version

-- c.
hundredsDigit :: (Integral a) => a -> a
hundredsDigit = snd . (`divMod` 10) . fst . (`divMod` 100)

foldBool :: a -> a -> Bool -> a
foldBool a b bool = case (a, b, bool) of
  (a, _, False) -> a
  (_, b, True) -> b

foldBool' :: a -> a -> Bool -> a
foldBool' x y bool
  | not bool = x
  | bool = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripeMain = do
  print $ roundTrip 4 == True
  print 4

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show
