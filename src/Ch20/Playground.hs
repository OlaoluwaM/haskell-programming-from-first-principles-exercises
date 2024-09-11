module Ch20.Playground where

import Data.Bool (bool)
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a = foldr (\member res -> a == member || res) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr f Nothing
  where
    f a = \case
        Just m -> Just $ min m a
        Nothing -> Just a

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr f Nothing
  where
    f a = \case
        Just m -> Just $ max m a
        Nothing -> Just a

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ -> const False) True

length' :: (Foldable t, Num b) => t a -> b
length' = foldr (\_ b -> b + 1) 0

toList' :: (Foldable t) => t a -> [a]
toList' = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr ((<>) . f) mempty

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF predicateFn = foldMap (\a -> bool mempty (pure a) $ predicateFn a)
