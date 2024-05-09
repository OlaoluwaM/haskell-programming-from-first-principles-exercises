module Ch14.ExerciseSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Data.Bifunctor (second)
import Data.Char (toUpper)
import Data.List qualified as List

half :: (Fractional a) => a -> a
half = (/ 2)

listOrdered :: (Ord a) => [a] -> Bool
listOrdered = snd . foldr go (Nothing, True)
 where
  go _ status@(_, False) = status
  go y (Nothing, t) = (Just y, t)
  go y (Just x, _) = (Just y, x >= y)

plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

multAssociative :: Integer -> Integer -> Integer -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

multCommutative :: Integer -> Integer -> Bool
multCommutative x y = x * y == y * x

square :: (Num a) => a -> a
square x = x * x

squareIdentity :: (Floating a) => a -> a
squareIdentity = square . sqrt

twice :: (a -> a) -> a -> a
twice f = f . f

fourTimes :: (a -> a) -> a -> a
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (firstLetter : restOfWord) = toUpper firstLetter : restOfWord

data Fool = Fulse | Frue deriving (Eq, Show)

foolEqChance :: Gen Fool
foolEqChance = oneof $ fmap pure [Fulse, Frue]

foolSkewedChance :: Gen Fool
foolSkewedChance = frequency $ fmap (second pure) [(2, Fulse), (1, Frue)]

spec :: Spec
spec = describe "Using QuickCheck" $ do
  prop "half identity" $ do
    \x -> ((* 2) . half) x `shouldBe` (x :: Double)

  prop "list ordered" $
    \xs -> (listOrdered . List.sort) (xs :: [Integer]) `shouldBe` True

  prop "plus associative" $ do
    \x y z -> plusAssociative x y z `shouldBe` True

  prop "plus commutative" $ do
    \x y -> plusCommutative x y `shouldBe` True

  prop "multiplication associative" $ do
    \x y z -> multAssociative x y z `shouldBe` True

  prop "multiplication commutative" $ do
    \x y -> multCommutative x y `shouldBe` True

  prop "quot to rem" $ do
    \x (NonZero y) -> (quot x y * y + rem x y) == (x :: Integer)

  prop "div to mod" $ do
    \x (NonZero y) -> (div x y * y + mod x y) == (x :: Integer)

  -- False, fails due to special exponent semantics around zeros, ones and negative integers
  prop "(^) not commutative" $ expectFailure $ do
    \x y -> x ^ y == (y :: Integer) ^ (x :: Integer)

  -- False, fails due to special exponent semantics around zero
  prop "(^) not associative" $ expectFailure $ do
    \x y z -> x ^ (y ^ z) == ((x :: Integer) ^ (y :: Integer)) ^ (z :: Integer)

  prop "reverse identity" $ do
    \xs -> (List.reverse . List.reverse) (xs :: [String]) == id xs

  prop "$ definition" $ do
    \(Fn f) x -> ((f :: Integer -> String) $ x) == f (x :: Integer)

  prop "(.) definition" $ do
    \(Fn f) (Fn g) x -> ((f :: String -> Bool) . (g :: Integer -> String)) (x :: Integer) == f (g x)

  prop "foldr (:) == (++)" $ do
    \a b -> foldr (:) (a :: String) b == ((b :: String) ++ a)

  prop "foldr (++) [] == concat" $ do
    \xs -> foldr (++) [] (xs :: [String]) == concat xs

  -- False, `take` returns an empty list if it is charged with taking *from* an empty list
  -- In those cases `length` will always yield 0 regardless of the value of `n`
  -- So, as long as this, `xs == []`, is possible, the PBT below won't pass
  -- It also won't pass if n can be < 0 or if the length of xs is less than n
  prop "length and take" $ expectFailure $ do
    \n xs -> length (take n (xs :: [Int])) == (n :: Int)

  prop "show read identity" $ do
    \x -> read (show x) == (x :: Integer)

  -- This fails due to inherent limitations with floating-point precision and arithmetics
  -- space constraints forcing rounding errors, etc...
  prop "square identity" $
    expectFailure $
      \(NonNegative x) -> squareIdentity x == (x :: Double)

  prop "idempotency capitalizeWord" $
    \x -> (capitalizeWord x == twice capitalizeWord (x :: String)) && (capitalizeWord x == fourTimes capitalizeWord x)

  prop "idempotency sort" $
    \xs -> (List.sort xs == twice List.sort (xs :: [Integer])) && (List.sort xs == fourTimes List.sort xs)
