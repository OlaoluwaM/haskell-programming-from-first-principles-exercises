module Ch25.ChapterEx where

import Data.Bifunctor

-- TODO: Use the checkers library to write tests for these instances

newtype Compose f g a = Compose {runCompose :: f (g a)} deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure = Compose . pure . pure

    (Compose fgf) <*> (Compose fga) = Compose $ liftA2 (<*>) fgf fga

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

data Deux a b = Deux a b deriving (Eq, Show)

instance Functor (Deux a) where
    fmap f (Deux a b) = Deux a (f b)

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

newtype Const a b = Const a

instance Functor (Const a) where
    fmap _ (Const a) = Const a

instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Functor (Drei a b) where
    fmap f (Drei a b c) = Drei a b (f c)

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Functor (SuperDrei a b) where
    fmap _ (SuperDrei a b) = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

newtype SemiDrei a b c = SemiDrei a

instance Functor (SemiDrei a b) where
    fmap _ (SemiDrei a) = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Functor (Quadriceps a b c) where
    fmap f (Quadzzz a b c d) = Quadzzz a b c (f d)

instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
    fmap _ (Left' e) = Left' e
    fmap f (Right' a) = Right' (f a)

instance Bifunctor Either' where
    bimap f _ (Left' e) = Left' (f e)
    bimap _ g (Right' a) = Right' (g a)
