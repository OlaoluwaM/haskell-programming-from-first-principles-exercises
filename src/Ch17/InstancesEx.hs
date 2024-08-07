{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch17.InstancesEx where

import Data.Monoid (Product, Sum)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Checkers

data Pair a = Pair a a deriving (Eq, Show, Generic, EqProp)

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure a = Pair a a
    (Pair f f') <*> (Pair a a') = Pair (f a) (f' a')

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

examplePair :: Pair (Int, Bool, String)
examplePair = undefined

data Two a b = Two a b deriving (Eq, Show, Generic, EqProp)

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance (Monoid a) => Applicative (Two a) where
    pure = Two mempty
    (Two a f) <*> (Two a' b) = Two (a <> a') (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

exampleTwo :: Two (Sum Int) (Int, Bool, Maybe String)
exampleTwo = undefined

data Three a b c = Three a b c deriving (Eq, Show, Generic, EqProp)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

exampleThree :: Three (Product Int) [Bool] (Int, Bool, Maybe String)
exampleThree = undefined

data Three' a b = Three' a b b deriving (Eq, Show, Generic, EqProp)

instance Functor (Three' a) where
    fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Monoid a) => Applicative (Three' a) where
    pure b = Three' mempty b b
    (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

exampleThree' :: Three' (Product Int) (Int, Bool, Maybe String)
exampleThree' = undefined

data Four a b c d = Four a b c d deriving (Eq, Show, Generic, EqProp)

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure = Four mempty mempty mempty
    (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

exampleFour :: Four (Product Int) [Bool] (Sum Int) (Int, Bool, Maybe String)
exampleFour = undefined

data Four' a b = Four' a a b b deriving (Eq, Show, Generic, EqProp)

instance Functor (Four' a) where
    fmap f (Four' a a' b b') = Four' a a' (f b) (f b')

instance (Monoid a) => Applicative (Four' a) where
    pure b = Four' mempty mempty b b
    (Four' a a' f f') <*> (Four' a'' a''' b b') = Four' (a <> a'') (a' <> a''') (f b) (f' b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

exampleFour' :: Four' (Product Int) (Int, Bool, Maybe String)
exampleFour' = undefined
