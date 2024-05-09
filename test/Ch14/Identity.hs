module Ch14.Identity where

import Test.QuickCheck

data Identity a = Identity a deriving (Eq, Show)

identityGen :: (Arbitrary a) => Gen (Identity a)
identityGen = Identity <$> arbitrary

-- Arbitrary instance for data wrapped in some container
instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity String)
identityGenInt = arbitrary

data Pair a b = Pair a b deriving (Eq, Show)

pairGen :: (Arbitrary a, Arbitrary b) => Gen (Pair a b)
pairGen = Pair <$> arbitrary <*> arbitrary

-- Arbitrary instance for product type
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = pairGen

pairGenIntString :: Gen (Pair Int String)
pairGenIntString = arbitrary

data Sum a b = First a | Second b deriving (Eq, Show)

sumGen :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGen = do
  a <- arbitrary
  b <- arbitrary
  oneof [pure $ First a, pure $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGen

sumGenFirstPls :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenFirstPls = do
  a <- arbitrary
  b <- arbitrary
  frequency [(10, pure $ First a), (1, pure $ Second b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = sumGenFirstPls

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = arbitrary
