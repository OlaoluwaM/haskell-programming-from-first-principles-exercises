module Ch11.Playground where

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

-- instance (Num a, TooMany a) => TooMany (a, a) where
--   tooMany :: (a, a) -> Bool
--   tooMany (n, n2) = tooMany (n + n2)

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

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

insert' :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert' a Leaf = Node a Leaf Leaf
insert' a bTree@(Node a' left right)
  | a == a' = bTree
  | a > a' = Node a left (insert' a right)
  | a < a' =
      Node
        a
        (insert' a left)
        right

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node a leftBTree rightBTree) = Node (f a) (mapTree f leftBTree) (mapTree f rightBTree)

testTree' :: BinaryTree Integer
testTree' = Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)

mapExpectedTree :: BinaryTree Integer
mapExpectedTree = Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node a leftBTree rightBTree) = a : (preOrder leftBTree ++ preOrder rightBTree)

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node a leftBTree rightBTree) = inOrder leftBTree ++ (a : inOrder rightBTree)

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node a leftBTree rightBTree) = postOrder leftBTree ++ postOrder rightBTree ++ [a]

testTree :: BinaryTree Integer
testTree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

testPreorder :: IO ()
testPreorder =
  if preOrder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inOrder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postOrder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "Bad news bears"

conversionCheck :: IO ()
conversionCheck = do
  testPreorder
  testInorder
  testPostorder

foldrTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree _ b Leaf = b
foldrTree f b (Node a leftTree rightTree) = f a remainingTreeFold
 where
  remainingTreeFold = foldrTree f rightTreeFold leftTree
  rightTreeFold = foldrTree f b rightTree

foldrTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree' f b tree = foldr f b (inOrder tree)

foldrTree'' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree'' f b tree = foldr f b (preOrder tree)

foldrTree''' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldrTree''' f b tree = foldr f b (postOrder tree)
