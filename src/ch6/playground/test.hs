{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Test where

data Trivial = Trivial

-- instance Eq Trivial where
--   Trivial == Trivial = True -- This is an infix definition for the == operation on the Trivial data type

instance Eq Trivial where
  (==) Trivial Trivial = True

f :: Int -> Int -> String
a `f` b = show $ a + b

data DoW = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show)

data Date = Date DoW Int
  deriving (Show)

instance Eq DoW where
  (==) Monday Monday = True
  (==) Tuesday Tuesday = True
  (==) Wednesday Wednesday = True
  (==) Thursday Thursday = True
  (==) Friday Friday = True
  (==) Saturday Saturday = True
  (==) Sunday Sunday = True
  (==) _ _ = False

instance Ord DoW where
  compare Friday Friday = EQ
  compare Friday _ = GT
  compare _ Friday = LT
  compare _ _ = EQ

instance Eq Date where
  (==) (Date dayOfWeek n) (Date dayOfWeek' n') = dayOfWeek == dayOfWeek' && n == n'

-- instance Show DoW where
--   show Monday = "Monday"
--   show Tuesday = "Tuesday"
--   show Wednesday = "Wednesday"
--   show Thursday = "Thursday"
--   show Friday = "Friday"
--   show Saturday = "Saturday"
--   show Sunday = "Sunday"

-- instance Show Date where
--     show (Date dayOfWeek n) = show dayOfWeek ++ " " ++ show n ++ "th"

{-# HLINT ignore "Use newtype instead of data" #-}
data Identity a = Identity a

instance (Eq a) => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'
