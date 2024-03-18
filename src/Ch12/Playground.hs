{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Ch12.Playground where

import Data.Char (isUpperCase)

import Data.Bool (bool)
import Data.Either (fromLeft, isLeft, partitionEithers)

vowels :: String
vowels = "aeiou"

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe "The" = Nothing
notThe str = Just str

isTitleCase :: String -> Bool
isTitleCase "" = False
isTitleCase str = isUpperCase $ head str

replaceThe :: String -> String
replaceThe = unwords . go . words
 where
  go [] = []
  go ("the" : rest) = "a" : go rest
  go ("The" : rest) = "A" : go rest
  go (str : rest) = str : go rest

replaceThe' :: String -> String
replaceThe' = unwords . go . words
 where
  go [] = []
  go (str : rest) = case notThe str of
    Just notTheStr -> notTheStr : go rest
    Nothing -> if isTitleCase str then "A" : go rest else "a" : go rest

startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel str = head str `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go False 0 . words
 where
  go _ count [] = count
  go False count (str : rest) = go (str == "the" || str == "The") count rest
  go True count (str : rest) = let newCount = if startsWithVowel str then count + 1 else count in go False newCount rest

newtype Word' = Word' String deriving (Eq, Show)

newtype VowelCount = VowelCount Integer deriving (Eq, Show, Num)
newtype ConsonantCount = ConsonantCount Integer deriving (Eq, Show, Num)

vowelCountToInteger :: VowelCount -> Integer
vowelCountToInteger (VowelCount n) = n

consonantCountToInteger :: ConsonantCount -> Integer
consonantCountToInteger (ConsonantCount n) = n

isVowel :: Char -> Bool
isVowel = flip elem vowels

mkWord :: String -> Maybe Word'
mkWord word = bool (Just (Word' word)) Nothing $ areVowelsMore $ getVowelAndConsonantCount word
 where
  getVowelAndConsonantCount :: String -> (VowelCount, ConsonantCount)
  getVowelAndConsonantCount = foldr (\char (vowelCount, consonantCount) -> if isVowel char then (vowelCount + 1, consonantCount) else (vowelCount, consonantCount + 1)) (0, 0)

  areVowelsMore (vowelCount, consonantCount) = vowelCountToInteger vowelCount > consonantCountToInteger consonantCount

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = natToInteger nat + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n > 0 = Just Succ <*> integerToNat (n - 1)
  | otherwise = Nothing

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- mayyybee :: b -> (a -> b) -> Maybe a -> b
-- mayyybee defaultVal f ma = case ma of
--   Just a -> f a
--   Nothing -> defaultVal

fromMaybe :: a -> Maybe a -> a
fromMaybe fallbackVal = maybe fallbackVal id

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes mas = map (fromMaybe undefined) $ filter isJust mas

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\ma -> (<*>) ((:) <$> ma)) (Just [])

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' [] = Just []
flipMaybe' (ma : mas) = ((:) <$> ma) <*> flipMaybe mas

lefts :: [Either a b] -> [a]
lefts = fst . partitionEithers

lefts' :: [Either a b] -> [a]
lefts' = foldr foldFn []
 where
  foldFn (Left a) b = a : b
  foldFn (Right _) b = b

lefts'' :: [Either a b] -> [a]
lefts'' x = [a | Left a <- x]

rights :: [Either a b] -> [b]
rights = snd . partitionEithers

rights' :: [Either a b] -> [b]
rights' = foldr foldFn []
 where
  foldFn (Right b) b' = b : b'
  foldFn (Left _) b = b

rights'' :: [Either a b] -> [b]
rights'' x = [b | Right b <- x]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr foldFn ([], [])
 where
  foldFn (Left a) (leftsL, rightsL) = (a : leftsL, rightsL)
  foldFn (Right b) (leftsL, rightsL) = (leftsL, b : rightsL)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f = \case
  Right b -> Just (f b)
  Left _ -> Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lF lR = \case
  Left a -> lF a
  Right b -> lR b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
  Just (a, b') -> a : myUnfoldr f b'
  Nothing -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f a = case f a of
  Just (a', b, a'') -> Node b (unfoldTree f a') (unfoldTree f a'')
  Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree (f n) 0
  where
    f n' a
      | a == n' = Nothing
      | otherwise = Just (a + 1, a, a + 1)
