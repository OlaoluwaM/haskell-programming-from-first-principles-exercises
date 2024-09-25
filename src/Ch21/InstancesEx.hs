{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch21.InstancesEx where

import Data.Monoid
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (arbitrary), frequency, listOf)
import Test.QuickCheck.Checkers

newtype Identity a = Identity a deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance Foldable Identity where
    foldr f acc (Identity a) = f a acc

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity a') = Identity (a <> a')

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
    foldr _ acc (Constant _) = acc

instance Traversable (Constant a) where
    traverse _ (Constant a) = pure $ Constant a

instance (Arbitrary a) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

data Optional a = Nada | Yep a deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
    pure = Yep
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    (Yep f) <*> (Yep a) = Yep (f a)

instance Foldable Optional where
    foldr _ acc Nada = acc
    foldr f acc (Yep a) = f a acc

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance (Arbitrary a) => Arbitrary (Optional a) where
    arbitrary = frequency [(1, pure Nada), (2, Yep <$> arbitrary)]

data List a = Nil | Cons a (List a) deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a rest) = Cons (f a) (fmap f rest)

instance Foldable List where
    foldr _ acc Nil = acc
    foldr f acc (Cons a rest) = foldr f (f a acc) rest

instance Traversable List where
    traverse _ Nil = pure Nil
    traverse f (Cons a rest) = Cons <$> f a <*> traverse f rest

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = foldr Cons Nil <$> listOf arbitrary

data Three a b c = Three a b c deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure = Three mempty mempty
    (Three a b f) <*> (Three a' b' c) = Three (a <> a') (b <> b') (f c)

instance Foldable (Three a b) where
    foldr f acc (Three _ _ c) = f c acc

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three a b <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Pair a b = Pair a b deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor (Pair a) where
    fmap f (Pair a b) = Pair a (f b)

instance (Monoid a) => Applicative (Pair a) where
    pure = Pair mempty
    (Pair a f) <*> (Pair a' b) = Pair (a <> a') (f b)

instance Foldable (Pair a) where
    foldr f acc (Pair _ b) = f b acc

instance Traversable (Pair a) where
    traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
    arbitrary = Pair <$> arbitrary <*> arbitrary

data Big a b = Big a b b deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor (Big a) where
    fmap f (Big a b b') = Big a (f b) (f b')

instance (Monoid a) => Applicative (Big a) where
    pure b = Big mempty b b

    (Big a f f') <*> (Big a' b b') = Big (a <> a') (f b) (f' b')

instance Foldable (Big a) where
    foldr f acc (Big _ b b') = f b' (f b acc)

instance Traversable (Big a) where
    traverse f (Big a b b') = Big a <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

data Bigger a b = Bigger a b b b deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor (Bigger a) where
    fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')

instance (Monoid a) => Applicative (Bigger a) where
    pure b = Bigger mempty b b b

    (Bigger a f f' f'') <*> (Bigger a' b b' b'') = Bigger (a <> a') (f b) (f' b') (f'' b'')

instance Foldable (Bigger a) where
    foldr f acc (Bigger _ b b' b'') = f b'' (f b' (f b acc))

instance Traversable (Bigger a) where
    traverse f (Bigger a b b' b'') = Bigger a <$> f b <*> f b' <*> f b''

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
    arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

data S n a = S (n a) a deriving (Eq, Ord, Show, Generic, EqProp)

instance (Functor n) => Functor (S n) where
    fmap f (S n a) = S (fmap f n) (f a)

instance (Applicative n) => Applicative (S n) where
    pure a = S (pure a) a

    (S nf f) <*> (S n a) = S (nf <*> n) (f a)

instance (Foldable n) => Foldable (S n) where
    foldr f acc (S n a) = foldr f (f a acc) n

instance (Traversable n) => Traversable (S n) where
    traverse f (S n a) = S <$> traverse f n <*> f a

instance (Functor n, Arbitrary (n a), Arbitrary a) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node t a t') = Node (fmap f t) (f a) (fmap f t')

instance Foldable Tree where
    foldr _ acc Empty = acc
    foldr f acc (Leaf a) = f a acc
    foldr f acc (Node t a t') = foldr f (f a (foldr f acc t')) t

instance Traversable Tree where
    traverse _ Empty = pure Empty
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node t a t') = Node <$> traverse f t <*> f a <*> traverse f t'

instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = frequency [(1, pure Empty), (2, Leaf <$> arbitrary), (3, Node <$> arbitrary <*> arbitrary <*> arbitrary)]
