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
