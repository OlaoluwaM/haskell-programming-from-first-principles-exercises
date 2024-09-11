module Ch18.Playground where

-- import Control.Monad (join)

-- exampleBind :: Monad m => (a -> m b) -> m a -> m b
-- exampleBind f = join . fmap f

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (First a) <*> _ = First a
    _ <*> (First a) = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second b) >>= f = f b

j :: (Monad m) => m (m a) -> m a
j mma = mma >>= id

l1 :: (Monad m) => (a -> b) -> m a -> m b
l1 = (<$>)

l2 :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = ma >>= (<$> mb) . f

a :: (Monad m) => m a -> m (a -> b) -> m b
a ma fab = fab >>= (<$> ma)

meh :: (Monad m) => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
meh (a' : as) famb = famb a' >>= \b -> (b :) <$> meh as famb

flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id
