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

