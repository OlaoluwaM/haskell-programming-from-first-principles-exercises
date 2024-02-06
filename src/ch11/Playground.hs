module Playground where

data Price = Price Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

myCar :: Vehicle
myCar = Car Mini (Price 14000)

urCar :: Vehicle
urCar = Car Mazda (Price 20000)

clownCar :: Vehicle
clownCar = Car Tata (Price 7000)

doge :: Vehicle
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane = not . isCar

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Maybe Manufacturer
getManu (Car manufacturer _) = Just manufacturer
getManu _ = Nothing

data PlaneSize = PlanetSize Float deriving (Eq, Show)
data Vehicle' = Car' Manufacturer Price | Plane' Airline PlaneSize
  deriving (Eq, Show)

myCar' :: Vehicle
myCar' = Car Mini (Price 14000)

urCar' :: Vehicle
urCar' = Car Mazda (Price 20000)

clownCar' :: Vehicle
clownCar' = Car Tata (Price 7000)

doge' :: Vehicle
doge' = Plane PapuAir

newtype Goats = Goats Int deriving (Show, Eq, Ord)

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany :: Int -> Bool
  tooMany = (> 42)

instance TooMany Goats where
  tooMany :: Goats -> Bool
  tooMany (Goats n) = n > 43

newtype Foo = Foo (Int, String) deriving (Show, Eq, Ord)

instance TooMany Foo where
  tooMany :: Foo -> Bool
  tooMany (Foo (n, _)) = tooMany n

newtype GoatField = GoatField (Goats, Goats) deriving (Show, Eq, Ord)

instance TooMany GoatField where
  tooMany :: GoatField -> Bool
  tooMany (GoatField (Goats n1, Goats n2)) = tooMany (n1 + n2)

instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany :: (a, a) -> Bool
  tooMany (n, n2) = tooMany (n + n2)

data Gardenia = Gardenia deriving (Show)
data Daisy = Daisy deriving (Show)
data Rose = Rose deriving (Show)
data Lilac = Lilac deriving (Show)

data FlowerType = Flower1 Gardenia | Flower2 Daisy | Flower3 Rose | Flower4 Lilac deriving (Show)

type Gardener = String

data Garden = Garden Gardener FlowerType deriving (Show) -- Cardinality is Infinite

-- Q: What is the sum of products normal form of Garden?

data Garden' = Garden1 Gardener Gardenia | Garden2 Gardener Daisy | Garden3 Gardener Rose | Garden4 Gardener Lilac deriving (Show)

data Garden'' = Gardenia' Gardener | Daisy' Gardener | Rose' Gardener | Lilac' Gardener deriving (Show)

data GuessWhat = ChickenButt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show) -- Represents an arbitrary product combining two types
data Sum a b = First a | Second b deriving (Eq, Show) -- Represents an arbitrary sum with two members
data RecordProduct a b = RecordProduct {pFirst :: a, pSecond :: b} deriving (Eq, Show)

data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer {os :: OperatingSystem, lang :: ProgLang} deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]

data Automobile = Null | Car'' {make :: String, model :: String, year :: Integer} deriving (Eq, Show)

-- This becomes possible
aCarModel :: String
aCarModel = model Null
