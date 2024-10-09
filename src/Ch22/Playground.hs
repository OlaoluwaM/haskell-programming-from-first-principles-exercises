{-# LANGUAGE TupleSections #-}
module Ch22.Playground where

import Data.Char
import Data.Functor ((<&>))

boop :: Integer -> Integer
boop = (* 2)

doop :: Integer -> Integer
doop = (+ 10)

bip :: Integer -> Integer
bip = boop . doop

bbop :: Integer -> (Integer, Integer)
bbop = (,) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  pure (a, b)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= \a -> rev <&> (a,)

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra)

instance Applicative (Reader r) where
  pure a = Reader (const a)

  (Reader rf) <*> (Reader ra) = Reader $ \r -> let f = rf r in  f . ra $ r

instance Monad (Reader r) where
  (Reader ra) >>= fra = Reader $ \r -> (runReader . fra . ra $ r) r

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb

asks :: (r -> a) -> Reader r a
asks = Reader

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person = Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
  } deriving (Eq, Show)

data Dog = Dog {
      dogsName :: DogName
    , dogsAddress :: Address
  } deriving (Eq, Show)

getDogRM :: Reader Person Dog
getDogRM = do
    name <- asks dogName
    addy <- asks address
    pure $ Dog name addy

getDogRA :: Reader Person Dog
getDogRA = liftA2 Dog (asks dogName) (asks address)
