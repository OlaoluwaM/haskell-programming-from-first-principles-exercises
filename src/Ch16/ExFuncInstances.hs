module Ch16.ExFuncInstances where

import Test.QuickCheck (Arbitrary (arbitrary))

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

-- No functor instance for this type constant because it is of kind `Type`
data Trivial = Trivial
