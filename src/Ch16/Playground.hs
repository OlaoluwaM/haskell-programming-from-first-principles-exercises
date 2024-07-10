module Ch16.Playground where

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers (f a)
    fmap _ LolNope = LolNope

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (Second b) = Second (f b)
    fmap _ (First a) = First a

-- We can apply f to `First` because it's been made a part of the functorial context, that is, the structure we wish to preserve during our transformation
