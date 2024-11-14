module Ch25.Playground where

-- TODO: Use the checkers library to write tests for these instances
-- Skip checks for the Compose' newtype

newtype One f a = One {getOne :: f a} deriving (Eq, Show)

newtype Compose' f g a = Compose' {runCompose' :: f (g a)} deriving (Eq, Show)

newtype Three f g h a = Three {getThree :: f (g (h a))} deriving (Eq, Show)

newtype Four f g h j a = Four {getFour :: f (g (h (j a)))} deriving (Eq, Show)

instance (Functor f) => Functor (One f) where
    fmap f (One fa) = One $ fmap f fa

instance (Functor f, Functor g) => Functor (Compose' f g) where
    fmap f (Compose' fga) = Compose' $ (fmap . fmap) f fga

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
    fmap f (Three fgha) = Three $ (fmap . fmap . fmap) f fgha

instance (Functor f, Functor g, Functor h, Functor j) => Functor (Four f g h j) where
    fmap f (Four fghja) = Four $ (fmap . fmap . fmap . fmap) f fghja

instance (Applicative f) => Applicative (One f) where
    pure = One . pure

    (One ff) <*> (One fa) = One $ ff <*> fa

instance (Applicative f, Applicative g) => Applicative (Compose' f g) where
    pure = Compose' . pure . pure

    (Compose' fgf) <*> (Compose' fga) = Compose' $ liftA2 (<*>) fgf fga

instance (Applicative f, Applicative g, Applicative h) => Applicative (Three f g h) where
    pure = Three . pure . pure . pure

    (Three fghf) <*> (Three fgha) = Three $ liftA2 (liftA2 (<*>)) fghf fgha

instance (Applicative f, Applicative g, Applicative h, Applicative j) => Applicative (Four f g h j) where
    pure = Four . pure . pure . pure . pure

    (Four fghjf) <*> (Four fghja) = Four $ liftA2 (liftA2 (liftA2 (<*>))) fghjf fghja
