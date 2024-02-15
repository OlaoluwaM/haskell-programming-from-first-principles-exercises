Chapter 11 Exercises
========================

Exercise: Dog Types
===========================

$$
\begin{code}
  data Doggies a = Husky a | Mastiff a deriving (Eq, Show)
\end{code}
$$

1. Q: Is $Doggies$ a type constructor or a data constructor? | A: It is a type constructor

2. Q: What is the kind of Doggies? | A: The type constructor $Doggies$ is of the kind $Type -> Type$

3. Q: What is the kind of $Doggies String$? | A: $Type$

4. Q: What is the type of $Husky 10$ | A: $Num a => Doggies a$

5. Q: What is the type of $Husky (10 :: Integer)$? | A: $Doggies Integer$

6. Q: What is the type of $Mastiff "Scooby Doo"$ | A: $Doggies String$

7. Q: Is $DogueDeBordeaux$ a type constructor or a data constructor | A: Both

8. Q: What is the type of $DogueDeBordeaux$ | A: $doge -> DogueDeBordeaux doge$

9. Q: What is the type of DogueDeBordeaux "doggie!" | A: $DogueDeBordeaux String$


Exercise: Vehicles
===========================

$$
\begin{code}
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
\end{code}
$$

1. Q: What is the type of $myCar$ | A: $Vehicle$

2. Given the following, define the functions

$$
\begin{code}
  isCar :: Vehicle -> Bool
  isCar (Car _ _) = True
  isCar         _ = False

  isPlane :: Vehicle -> Bool
  isPlane = not . isCar

  areCars :: [Vehicle] -> [Bool]
  areCars = map isCar
\end{code}
$$

3. Now, we’re going to write a function to tell us the manufacturer of a piece of data:

$$
\begin{code}
  getManu :: Vehicle -> Maybe Manufacturer
  getManu (Car manufacturer _) = Just manufacturer
  getManu _ = Nothing

  getManuPartial :: Vehicle -> Manufacturer
  getManuPartial (Car manufacturer _) = manufacturer
\end{code}
$$

4. Given that we’re returning the Manufacturer, what will happen if you use this on Plane data?

Such an expression will be a $bottom$ since the $getManuPartial$ function does have the means to handle a $Vehicle$ that isn't a car. An exception will be thrown by the runtime

5. All right. Let’s say you decide to add the size of the plane as an argument to the Plane constructor. Add that to your datatypes in the appropriate places, and change your data and functions appropriately.

$$
\begin{code}
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

  isCar' :: Vehicle -> Bool
  isCar' (Car _ _) = True
  isCar' _ = False

  isPlane' :: Vehicle -> Bool
  isPlane' = not . isCar

  areCars' :: [Vehicle] -> [Bool]
  areCars' = map isCar

  getManu' :: Vehicle -> Maybe Manufacturer
  getManu' (Car manufacturer _) = Just manufacturer
  getManu' _ = Nothing

  getManuPartial' :: Vehicle -> Manufacturer
  getManuPartial' (Car manufacturer _) = manufacturer
\end{code}
$$

Exercises : Cardinality
===========================

1. Q: $data PugType = PugData$ | A: 1

2. For this one, recall that Bool is also defined with the '|' symbol

> data Airline'' = PapuAir'' | CatapultsR'Us'' | TakeYourChancesUnited''

Cardinality is 3

3. Q: Given what we know about Int8, what’s the cardinality of Int16? | A: 65536

4. Use the REPL and maxBound and minBound to examine $Int$ and $Integer$. What can you say about the cardinality of those types?

Since the $Int$ type is a finite set, with a preponderance of values. it's cardinality is 1.844674407×10¹⁹ capped by it's allocated size of 64 bits.Airline

$Integer$ on the other hand is unbounded because it's size constraint isn't as concrete as other numeric types, hence it's cardinality is seemingly infinite, but is capped by other factors in practice

5. Extra credit (impress your friends!): what’s the connection between the 8 in Int8 and that type’s cardinality of 256?

The type $Int8$ is limited by its allotted size of 8 bits, hence why it's *width* is 8, which can be utilized to repreesent up to 256 values

Exercises : For example
===========================

1. What is the type of the data constructor $MakeExample$? What happens when you request the type of $Example$?

THe type of the data constant (nullary data constructor) $MakeExample$ is $Example$. If you try to request the type of the type constant (nullary type constructor) $Example$ you'll get an error as a type cannot belong to another type, but it *can* have a kind

2. What if you try :info on $Example$ in GHCi? Can you determine what type class instances are defined for the $Example$ type using :info in GHCi?

Only $Show$

3. Try making a new datatype like $Example$ but with a single type argument added to $MakeExample$, such as $Int$. What has changed when you query $MakeExample$ with :type in GHCi?

$MakeExample$ has become a unary data constructor so it's type is akin to that of a function, a function from the type of it's parameter, $Int$, to $Example$

Exercises : Logic goats
===========================

1. Reusing the $TooMany$ type class, write an instance of the type class for the type $(Int, String)$. This will require adding a language pragma named FlexibleInstances if you do not use a newtype— GHC will tell you what to do.

$$
\begin{code}
newtype Foo = Foo (Int, String) deriving (Show, Eq, Ord)

instance TooMany Foo where
  tooMany :: Foo -> Bool
  tooMany (Foo (n, _)) = tooMany n
\end{code}
$$

2. Make another $TooMany$ instance for $(Int, Int)$. Sum the values together under the assumption that this is a count of goats from two fields.

$$
\begin{code}
newtype GoatField = GoatField (Goats, Goats) deriving (Show, Eq, Ord)

instance TooMany GoatField where
  tooMany :: GoatField -> Bool
  tooMany (GoatField (Goats n1, Goats n2)) = tooMany (n1 + n2)
\end{code}
$$

3. Make another $TooMany$ instance, this time for $(Num a, TooMany a) => (a, a)$. This can mean whatever you want, such as summing the two numbers together.

$$
\begin{code}
instance (Num a, TooMany a) => TooMany (a, a) where
  tooMany :: (a, a) -> Bool
  tooMany (n, n2) = tooMany (n + n2)
\end{code}
$$

Exercises : Pithy the Bool
===========================

1. Given a datatype:

> data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

Q: What is its cardinality? Hint: We already know Bool’s cardinality. Show your work, as demonstrated earlier.

A: 4

2. Given a datatype:

> import Data.Int
> data NumberOrBool = Numba Int8 | BoolyBool Bool deriving (Eq, Show)
> myNumba = Numba (-128)

Q: What is the cardinality of $NumberOrBool$

A: 258

Exercises : How does your garden grow?
===========================

$$
\begin{code}
data Gardenia = Gardenia deriving (Show)
data Daisy = Daisy deriving (Show)
data Rose = Rose deriving (Show)
data Lilac = Lilac deriving (Show)

data FlowerType = Flower1 Gardenia | Flower2 Daisy | Flower3 Rose | Flower4 Lilac deriving (Show)

-- type Gardener = String
data Gardener = Alice | Bob deriving (Show)

data Garden = Garden Gardener FlowerType deriving (Show) -- Cardinality is Infinite

-- Q: What is the sum of products normal form of Garden?

data Garden' = Garden1 Gardener Gardenia | Garden2 Gardener Daisy | Garden3 Gardener Rose | Garden4 Gardener Lilac deriving (Show)

-- Alternatively, we could also rewrite Garden as

data Garden'' = Gardenia' Gardener | Daisy' Gardener | Rose' Gardener | Lilac' Gardener deriving (Show)
\end{code}
$$

Exercises : Programmers
===========================

$$
\begin{code}
data OperatingSystem = GnuPlusLinux | OpenBSDPlusNevermindJustBSDStill | Mac | Windows deriving (Eq, Show)
data ProgLang = Haskell | Agda | Idris | PureScript deriving (Eq, Show)

data Programmer = Programmer {os :: OperatingSystem, lang :: ProgLang} deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux, OpenBSDPlusNevermindJustBSDStill, Mac, Windows]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer os lang | os <- allOperatingSystems, lang <- allLanguages]
\end{code}
$$

Exercises : The Quad
===========================

$$
\begin{code}
data Quad =
    One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- Cardinality of the type "Quad" is 4
\end{code}
$$

1. Q: What is the cardinality of the type $Either Quad Quad$ | A: 4 + 4, 8

2. Q: What is the cardinality of the type $(Quad, Quad)$ | A: 4 * 4, 16

3. Q: What is the cardinality of the type $Quad -> Quad$ | A: 4 ** 4, 256

4. Q: What is the cardinality of the type $(Bool, Bool, Bool)$ | A: 2 * 2 * 2, 8

5. Q: What is the cardinality of the type $Bool -> Bool -> Bool$ | A: 2 ^ (2 * 2), so 2 ^ 4, 16

6 Q: What is the cardinality of the type $Bool -> Quad -> Quad$ | A: 4 ^ (4 * 2), so 4 ^ 8, 65536

Write map for BinaryTree
===========================

$$
\begin{code}
data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node a leftBTree rightBTree) = Node (f a) (mapTree f leftBTree) (mapTree f rightBTree)

testTree' :: BinaryTree Integer
testTree' = Node 1 (Node 3 Leaf Leaf) (Node 4 Leaf Leaf)

mapExpectedTree :: BinaryTree Integer
mapExpectedTree = Node 2 (Node 4 Leaf Leaf) (Node 5 Leaf Leaf)

mapOkay =
  if mapTree (+1) testTree' == mapExpected
    then print "yup OK!"
    else error "test failed!"
\end{code}
$$

Convert binary trees to lists
===========================

$$
\begin{code}
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
\end{code}
$$

Write foldr for BinaryTree
===========================

$$
\begin{code}
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
\end{code}
$$

Chapter Exercises
===========================

Multiple Choice
==================

1. a
2. c
3. b
4. c
5.
