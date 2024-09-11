{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch18.InstancesEx where

import Control.Monad (ap, liftM)
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Data.Monoid

data Nope a = NopeDotJpg deriving (Eq, Show, Generic, EqProp)

instance Functor Nope where
    fmap = liftM

instance Applicative Nope where
    pure = const NopeDotJpg
    (<*>) = ap

instance Monad Nope where
    NopeDotJpg >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary = pure NopeDotJpg

exampleNope :: Nope (Int, Bool, Sum Integer)
exampleNope = undefined

data BahEither b a = PLeft a | PRight b deriving (Eq, Show, Generic, EqProp)

instance Functor (BahEither b) where
    fmap = liftM

instance Applicative (BahEither b) where
    pure = PLeft
    (<*>) = ap

instance Monad (BahEither b) where
    (PRight b) >>= _ = PRight b
    (PLeft a) >>= f = f a

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
    arbitrary = frequency [(3, PLeft <$> arbitrary), (1, PRight <$> arbitrary)]

exampleBahEither :: BahEither String (Bool, String, Int)
exampleBahEither = undefined

newtype Identity a = Identity a deriving (Eq, Show, Ord, Generic, EqProp)

instance Functor Identity where
    fmap = liftM

instance Applicative Identity where
    pure = Identity
    (<*>) = ap

instance Monad Identity where
    (Identity a) >>= f = f a

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

exampleIdentity :: Identity (Sum Int, String, Product Int)
exampleIdentity = undefined

data List a = Nil | Cons a (List a) deriving (Eq, Show, Generic, EqProp)

append :: List a -> List a -> List a
append Nil ys' = ys'
append (Cons x'' xs') ys' = Cons x'' $ xs' `append` ys'

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = concat' . fmap f

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a restOfList) = Cons (f a) (fmap f restOfList)

instance Applicative List where
    pure = flip Cons Nil
    Nil <*> Nil = Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    (Cons f fs) <*> k@(Cons n ns) = Cons (f n) (fmap f ns) `append` (fs <*> k)

instance Monad List where
    Nil >>= _ = Nil
    consVal@(Cons _ _) >>= f = flatMap f consVal

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = frequency [(3, Cons <$> arbitrary <*> arbitrary), (1, pure Nil)]

exampleList :: List (Int, String, Int)
exampleList = undefined
