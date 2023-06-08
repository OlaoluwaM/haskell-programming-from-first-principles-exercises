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

add :: (Num a) => a -> a -> a
add x y = x + y

data Person = Person Bool
  deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

data Mood = Woot | Blah
  deriving (Show, Eq)

settleDown x = if x == Woot then Blah else x

type Subject = String

type Verb = String

type Object = String

data Sentence = Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"

s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

truth = Papu (Rocks "Cho") (Yeah True)

equalityForAll :: Papu -> Papu -> Bool
equalityForAll p p' = p == p
