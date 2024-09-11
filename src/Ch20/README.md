# Chapter 20

## Exercise: Library functions

Implement the functions in terms of `foldMap` or `foldr` from Foldable, then try them out with multiple types that have Foldable instances.

```haskell
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
```

## Section 20.6

Write Foldable instances for the following datatypes:

[Instance definitions can be found here](./InstancesEx.hs), and the code checking the lawfulness of those definition is [here](../../test/Ch20/InstancesExSpec.hs)
