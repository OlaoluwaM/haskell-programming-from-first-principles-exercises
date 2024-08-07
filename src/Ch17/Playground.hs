{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch17.Playground where

import Control.Applicative (liftA3)
import Data.List qualified as L
import Test.QuickCheck.Checkers (EqProp)

import GHC.Generics

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

newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show, Ord)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a) <*> (Constant b) = Constant (a <> b)

validateLength :: Int -> String -> Maybe String
validateLength maxLen s = if length s > maxLen then Nothing else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s = Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = Address <$> validateLength 100 a

data Person = Person Name Address deriving (Eq, Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

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

newtype ZipList' a = ZipList' [a] deriving (Eq, Show, Generic, EqProp)

instance Functor ZipList' where
    fmap f (ZipList' l) = ZipList' $ fmap f l

instance Applicative ZipList' where
    pure = ZipList' . (: [])
    (ZipList' fs) <*> (ZipList' ls) = ZipList' $ zipWith ($) fs ls

data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance (Monoid e) => Applicative (Validation e) where
    pure = Success

    (Failure e) <*> (Failure e') = Failure (e <> e')
    (Failure e) <*> _ = Failure e
    _ <*> (Failure e) = Failure e
    (Success f) <*> (Success a) = Success (f a)

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)
