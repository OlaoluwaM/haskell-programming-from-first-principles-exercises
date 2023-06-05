{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module EqInstancesExercise where

  -- Exercise 1
  data TisAnInteger = TisAn Integer

  instance Eq TisAnInteger where
    (==) (TisAn n) (TisAn n') = n == n'

  -- Exercise 2
  data TwoIntegers = Two Integer Integer

  instance Eq TwoIntegers where
    (==) (Two n n') (Two n2 n2') = n == n2 && n' == n2'

  -- Exercise 3
  data StringOrInt = TisAnInt Int | TisAsString String

  instance Eq StringOrInt where
    (==) (TisAsString str) (TisAsString str') = str == str'
    (==) (TisAnInt n) (TisAnInt n') = n == n'
    (==) _ _ = False

  -- Exercise 4
  data Pair a = Pair a a

  instance Eq a => Eq (Pair a) where
    (==) (Pair x x') (Pair y y') = x == y && x' == y'

  -- Exercise 5
  data Tuple a b = Tuple a b

  instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple x y) (Tuple x' y') = x == x' && y == y'

  -- Exercise 6
  data Which a = ThisOne a | ThatOne a

  instance Eq a => Eq (Which a) where
    (==) (ThisOne x) (ThisOne x') = x == x'
    (==) (ThatOne x) (ThatOne x') = x == x'
    (==) _ _ = False

  -- Exercise 7
  data EitherOr a b = Hello a | Goodbye b

  instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello x') = x == x'
    (==) (Goodbye y) (Goodbye y') = y == y'
    (==) _ _ = False
