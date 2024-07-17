{-# LANGUAGE GADTs #-}

module Ch16.ExFuncInstances where

import Test.QuickCheck (Arbitrary (arbitrary))

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = arbitrary >>= (<$> arbitrary) . Pair

data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = arbitrary >>= (<$> arbitrary) . Two

data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        Three a b <$> arbitrary

data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        pure $ Three' a b b

data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        Four a b c <$> arbitrary

data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        Four' a a a <$> arbitrary

-- No functor instance for this type constant because it is of kind `Type`
data Trivial = Trivial

data Wrap f a where
    Wrap :: (f a) -> Wrap f a
    deriving (Eq, Show)

instance (Functor f) => Functor (Wrap f) where
    fmap f (Wrap f') = Wrap $ fmap f f'
