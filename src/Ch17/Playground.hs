module Ch17.Playground where

import Data.List qualified as L

added :: Maybe Integer
added = (+ 3) <$> lookup 3 (zip [1 .. 3] [4 .. 6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

x :: Maybe Int
x = L.elemIndex 3 [1 .. 5]

y' :: Maybe Int
y' = L.elemIndex 4 [1 .. 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y'

xs :: [Integer]
xs = [1 .. 3]

ys :: [Integer]
ys = [4 .. 6]

x' :: Maybe Integer
x' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x' <*> y'')

newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity (f a)

-- instance Applicative Identity where
--     pure = Identity

--     (Identity f) <*> idA = fmap f idA
