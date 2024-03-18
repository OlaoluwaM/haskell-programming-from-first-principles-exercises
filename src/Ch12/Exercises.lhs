import Data.Char (isUpperCase)
Chapter 12 Exercises
==============================

Determine the kinds
=====================

1. Q: Given `id :: a -> a` What is the kind of `a` | A: `a` is of kind `Type`/`*`

2 Q: Given `r :: a -> f a` What are the kinds of `a` and `f` | A: `a` is of kind `Type`/`*` while `f` is of kind `Type -> Type`

String processing
=====================

1. Write a recursive function named replaceThe that takes a text/string, breaks it into words, and replaces each instance of "the" with "a". It should only replace exactly the word "the". `notThe` is a suggested helper function for accomplishing this:

$$
\begin{code}
import Data.Char

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
    Nothing -> if isTitleCase str then "A" : go rest else "a" :  go rest

\end{code}
$$

2. Write a recursive function that takes a text/string, breaks it into words, and counts the number of instances of "the" followed by a vowel-initial word:

$$
\begin{code}
startsWithVowel :: String -> Bool
startsWithVowel "" = False
startsWithVowel str = let vowels = "aeiou" in head str `elem` vowels

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go False 0 . words
 where
  go _ count [] = count
  go False count (str : rest) = go (str == "the" || str == "The") count rest
  go True count (str : rest) = let newCount = if startsWithVowel str then count + 1 else count in go False newCount rest
\end{code}
$$

Validate the word
=====================

Use the Maybe type to write a function that counts the number of vowels in a string and the number of consonants. If the number of vowels exceeds the number of consonants, the function returns Nothing. In many human languages, vowels rarely exceed the number of consonants, so when they do, it may indicate the input isn’t a word (that is, a valid input to your dataset):

$$
\begin{code}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

vowels :: String
vowels = "aeiou"

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
\end{code}
$$

It's only natural
=====================

You’ll be presented with a datatype to represent the natural numbers. The only values representable with the naturals are whole numbers from zero to infinity. Your task will be to implement functions to convert natural numbers to integers and integers to naturals. The conversion from Nat to Integer won’t return Maybe, because—as you know—Integer is a strict superset of Nat. Any Nat can be represented by an Integer, but the same is not true of any Integer. Negative numbers are not valid natural numbers:

$$
\begin{code}
data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ nat) = natToInteger nat + 1

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n == 0 = Just Zero
  | n > 0 = Just Succ <*> integerToNat (n - 1)
  | otherwise = Nothing
\end{code}
$$

Small Library for Maybe
=====================

Write the following functions. This may take some time.

1. Simple Boolean checks for Maybe values:

$$
\begin{code}
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust
\end{code}
$$

2. The following is the Maybe catamorphism. You can turn a Maybe value into anything else with this:

$$
\begin{code}
mayyybee :: b -> (a -> b) -> Maybe a -> b
mayyybee defaultVal f ma = case ma of
  Just a -> f a
  Nothing -> defaultValue
\end{code}
$$

3. In case you just want to provide a fallback value. Try writing it in terms of the maybe catamorphism:

> fromMaybe :: a -> Maybe a -> a
> fromMaybe fallbackVal = mayyybee fallbackVal id

4. Converting between List and Maybe:

$$
\begin{code}
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]
\end{code}
$$

5. For when we want to drop the Nothing values from a list:

$$
\begin{code}
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes mas = map (fromMaybe undefined) $ filter isJust mas
\end{code}
$$

6. You’ll see this called sequence later:

$$
\begin{code}

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\ma -> (<*>) ((:) <$> ma)) (Just [])

flipMaybe' :: [Maybe a] -> Maybe [a]
flipMaybe' [] = Just []
flipMaybe' (ma : mas) = ((:) <$> ma) <*> flipMaybe mas
\end{code}
$$

Small Library for Either
=====================

Write each of the following functions. If more than one possible unique function exists for the type, use common sense to determine what it should do.

1. Try to eventually arrive at a solution that uses foldr, even if earlier versions don’t use foldr:

> lefts' :: [Either a b] -> [a]
> lefts' = foldr foldFn []
>  where
>   foldFn (Left a) b = a : b
>   foldFn (Right _) b = b

2. Same as the last one. Use foldr, eventually:

> rights' :: [Either a b] -> [b]
> rights' = foldr foldFn []
>  where
>   foldFn (Right b) b' = b : b'
>   foldFn (Left _) b = b

3. partitionEithers'

> partitionEithers' :: [Either a b] -> ([a], [b])
> partitionEithers' = foldr foldFn ([], [])
>  where
>   foldFn (Left a) (leftsL, rightsL) = (a : leftsL, rightsL)
>   foldFn (Right b) (leftsL, rightsL) = (leftsL, b : rightsL)

4. eitherMaybe'

> eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
> eitherMaybe' f = \case
>   Right b -> Just (f b)
>   Left _ -> Nothing

5. This is a general catamorphism for Either values:

> either' :: (a -> c) -> (b -> c) -> Either a b -> c
> either' lF lR = \case
>   Left a -> lF a
>   Right b -> lR b

6. Same as before, but use the either' function you just wrote:

> eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
> eitherMaybe'' f = either' (const Nothing) (Just . f)

Write your own iterate and unfoldr
=====================

1. Write the function myIterate using direct recursion. Compare the behavior with the built-in iterate to gauge correctness. Do not look at the source or any examples of iterate, so that you are forced to do this yourself:

> myIterate :: (a -> a) -> a -> [a]
> myIterate f a = a : myIterate f (f a)

2. Write the function myUnfoldr using direct recursion. Compare with the built-in unfoldr to check your implementation. Again, don’t look at implementations of unfoldr, so that you figure it out yourself:

> myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
> myUnfoldr f b = case f b of
>   Just (a, b') -> a : myUnfoldr f b'
>   Nothing -> []

3 . Rewrite myIterate into betterIterate using myUnfoldr . A hint — we use unfoldr to produce the same results as iterate above . Do this with different functions, and see if you can abstract the structure out. It helps to have the types in front of you:

> betterIterate :: (a -> a) -> a -> [a]
> betterIterate f = myUnfoldr (\b -> Just (b, f b))

Finally something other than a list
=====================

> data BinaryTree a = Leaf | Node a (BinaryTree a) (BinaryTree a) deriving (Eq, Show)

1. Write unfold for BinaryTree:

> unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
> unfoldTree f a = case f a of
>   Just (a', b, a'') -> Node b (unfoldTree f a') (unfoldTree f a'')
>   Nothing -> Leaf

2. Make a tree builder.

> treeBuild :: Integer -> BinaryTree Integer
> treeBuild n = unfoldTree (f n) 0
>  where
>   f n' a
>     | a == n' = Nothing
>     | otherwise = Just (a + 1, a, a + 1)
