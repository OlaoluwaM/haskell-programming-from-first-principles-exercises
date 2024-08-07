# Chapter 10 Exercises

## Exercise: Understanding Folds

1. `foldr (*) 1 [1..5]` Will return the same result as which of the following?

a) `flip (*) 1 [1..5]`
b) `foldl (flip (*)) 1 [1..5]`
c) `foldl (*) 1 [1..5]`

Answer: c & b.

2. Write out the evaluation steps for: `foldl (flip (*)) 1 [1..3]`

`filp (*)` is equivalent to `(*)` because multiplication is commutative

```haskell
foldl (*) 1 [1, 2, 3]
foldl (*) (1 *1) [2, 3]
foldl (*) ((1 * 1) * 2) [3]
foldl (*) (((1* 1) *2)* 3) []
(((1 *1)* 2) *3)
((1* 2) *3)
(2* 3)
6
```

3. One difference between `foldr` and `foldl` is:

a) `foldr`, but not `foldl`, traverses the spine of a list from right
to left.
b) `foldr`, but not `foldl`, always forces the rest of the fold.
c) `foldr`, but not `foldl`, associates to the right.
d) `foldr`, but not `foldl`, is recursive.

Answer: c.

4. Folds are catamorphisms, which means they are generally used to:

a) Reduce structure.
b) Expand structure.
c) Render you catatonic.
d) Generate infinite data structures.

Answer: a.

5.The following are simple folds very similar to what you’ve already seen, but each has at least one error. Please fix and test
them in your REPL:

```haskell
foldr (++) ["woot", "WOOT", "woot"]
foldr (++) "" ["woot", "WOOT", "woot"]

foldr max "fear is the little death"
foldr max 'a' "fear is the little death"

foldr and True [False, True]
foldr (&&) True [False, True]

foldr (||) True [False, True]
foldr (||) False [False, True]

foldl ((++) . show) "" [1..5]
foldr ((++) . show) "" [1..5]

foldr const 'a' [1..5]
foldr (flip const) 'a' [1..5]
foldl const 'a' [1..5]

foldr const 0 "tacos"
foldr (flip const) 0 "tacos"
foldl const 0 "tacos"

foldl (flip const) 0 "burritos"
foldl const 0 "burritos"
foldr (flip const) 0 "burritos"

foldl (flip const) 'z' [1..5]
foldl const 'z' [1..5]
foldr (flip const) 'z' [1..5]
```

## Exercise : Database Processing

```haskell
import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)), DbNumber 9001, DbString "Hello, world!", DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]
```

1. Write a function that filters for $DbDate$ values and returns a list of the $UTCTime$ values inside them:

```haskell
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr getDates []
 where
  getDates (DbDate date) acc = date : acc
  getDates _ acc = acc
```

1. Write a function that filters for $DbNumber$ values and returns a list of the Integer values inside them:

$$
\begin{code}
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr dBNumbers []
 where
  dBNumbers (DbNumber num) acc = num : acc
  dBNumbers _ acc = acc
\end{code}
$$

3. Write a function that gets the most recent date:

$$
\begin{code}
mostRecentDate :: [DatabaseItem] -> UTCTime
mostRecentDate = maximum . filterDbDate
\end{code}
$$

4. Write a function that sums all of the $DbNumber$ values:

$$
\begin{code}
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber
\end{code}
$$

5. Write a function that gets the average of the $DbNumber$ values:

$$
\begin{code}
avgDb :: [DatabaseItem] -> Double
avgDb dbIs = let dbNums = filterDbNumber dbIs in fromIntegral (fromInteger (sum dbNums) `div` length dbNums)
\end{code}
$$

Scan Exercises
==============

1. Modify your fibs function to only return the first 20 Fibonacci numbers.

> fibs' :: [Integer]
> fibs' = take 20 $ fibs

2. Modify fibs to return the Fibonacci numbers that are less than 100.

> fibs'' :: [Integer]
> fibs'' = takeWhile (< 100) fibs

3. Try to write the factorial function from Chapter 8 as a scan. You’ll want scanl again, and your start value will be 1. Warning: this will also generate an infinite list, so you may want to pass it through a take function or similar.

> factorials :: [Integer]
> factorials = scanl (*) 1 [2 ..]

Chapter Exercies
===============

Warm - up and review
-------------------

1. Given the following sets of consonants and vowels:

> stops = "pbtdkg"
> vowels = "aeiou"

a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combina- tions. These will not all correspond to real words in English, although the stop-vowel-stop pattern is common enough that many of them will.

> svs :: [(Char, Char, Char)]
> svs = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

b) Modify that function so that it only returns the combinations that begin with a p.

> svs' :: [(Char, Char, Char)]
> svs' = [(s1, v, s2) | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]

c) Now set up lists of nouns and verbs (instead of stops and vowels), and modify the function to make tuples representing possible noun-verb-noun sentences.

> nouns :: [String]
> nouns = ["Ola", "Andrew", "Dog", "Cat", "Monday", "Moon", "Sun", "Earth", "Laptop"]
>
> verbs :: [String]
> verbs = ["eat", "rehearse", "recite", "strike", "rise", "set", "speak", "promote", "take"]
>
> nvn :: [(String, String, String)]
> nvn = [(n1, v, n2) | n1 <- nouns, v <- verbs, n2 <- nouns]

2. What does the following mystery function do? What is its type? Try to get a good sense of what it does before you test it in the REPL to verify it:

> seekritFunc x = div (sum (map length (words x))) (length (words x))

$seekritFunc$ gets the average word length in a sentence

3. We'd really like the answer to be more precise. Can you rewrite that using fractional division?

> seekritFuncFractional :: String -> Double
> seekritFuncFractional x = sum (map (fromIntegral . length) (words x)) / (fromIntegral . length) (words x)

Rewriting functions using folds
-------------------------------

1. $myOr$ returns True if any Bool in the list is True:

> myOr :: [Bool] -> Bool
> myOr = foldr (||) False

2. $myAny$ returns $True$ if $a -> Bool$ applied to any of the values in the list returns $True$:

> myAny :: (a -> Bool) -> [a] -> Bool
> myAny f xs = foldr (\x acc -> f x || acc) False

3. Write two versions of $myElem$. One version should use folding and the other should use any:

> myElem :: (Eq a) => a -> [a] -> Bool
> myElem x xs = foldr (\x' _ -> x == x') False

4. Implement myReverse. Don't worry about trying to make it lazy.

> myReverse :: [a] -> [a]
> myReverse = foldl' (flip (:)) []

5. Write $myMap$ in terms of $foldr$. It should have the same behavior as the built-in map:

> myMap :: (a -> b) -> [a] -> [b]
> myMap f = foldr (\x acc -> f x : acc) []

6. Write $myFilter$ in terms of $foldr$. It should have the same behavior as the built-in filter:

> myFilter :: (a -> Bool) -> [a] -> [a]
> myFilter pred = foldr (\x acc -> if pred x then x : acc else acc) []

7. $squish$ flattens a list of lists into a list:

> squish :: [[a]] -> [a]
> squish = foldr (flip (++)) []

8. $squishMap$ maps a function over a list and concatenates the result:

> squishMap :: (a -> [b]) -> [a] -> [b]
> squishMap f = foldr ((++) . f) []

9. $squishAgain$ flattens a list of lists into a list. This time, re-use the $squishMap$ function:

> squishAgain :: [[a]] -> [a]
> squishAgain = squishMap id

10. $myMaximumBy$ takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returns GT for:

> myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
> myMaximumBy _[x] = x
> myMaximumBy compareFn (x : xs) = case compareFn x tailMax of
> GT -> x
>_ -> tailMax
> where
> tailMax = myMaximumBy compareFn xs

11. $myMinimumBy$ takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns LT for:

> myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
> myMinimumBy _[x] = x
> myMinimumBy compareFn (x : xs) = case compareFn x tailMin of
> LT -> x
>_ -> tailMin
> where
> tailMin = myMinimumBy compareFn xs
