# Chapter 9 Exercises

## Exercise: EnumFromTo

```haskell
eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True True = [True]
eftBool True False = []

-- data Ordering = LT | EQ | GT
eftOrdering :: Ordering -> Ordering -> [Ordering]
eftOrdering LT LT = [LT]
eftOrdering LT EQ = [LT, EQ]
eftOrdering LT GT = [LT, EQ, GT]
eftOrdering EQ EQ = [EQ]
eftOrdering EQ GT = [EQ, GT]
eftOrdering GT GT = [GT]
eftOrdering GT _ = []

eftInt :: Int -> Int -> [Int]
eftInt int1 int2
  | int1 > int2 = []
  | otherwise = go int1 int2 [int2]
 where
  go start stop enumerations
    | length enumerations == ((stop - start) + 1) = enumerations
    | otherwise = let count = (length enumerations) go start stop ( (stop - count) : enumerations)

eftChar :: Char -> Char -> [Char]
eftChar char1 char2
  | char1 > char2 = []
  | otherwise = go char1 char2 char2 [char2]
 where
  go startChar endChar currChar enumerations
    | startChar == currChar = enumerations
    | otherwise = let prevChar = pred currChar in go startChar endChar prevChar (prevChar : enumerations)
```

## Exercise: Thy Fearful Symmetry

1. Using `takeWhile` and `dropWhile`, write a function that takes a string and returns a list of strings, using spaces to separate the elements of the string into words, as in the following sample

    ```sh
    Prelude> myWords "sheryl wants fun"
    ["sheryl", "wants", "fun"]
    ```

    ```haskell
    myWords :: String -> [String]
    myWords [] = []
    myWords str
      | null word = []
      | otherwise = word : myWords rest
     where
      trim = dropWhile isSpace str
      word = takeWhile (not . isSpace) trim
      rest = dropWhile (not . isSpace) trim
    ```

2. Next, write a function that takes a string and returns a list of strings, using newline separators to break up the string, as in the following (your job is to fill in the undefined function)

    ```haskell
      myLines :: String -> [String]
      myLines [] = []
      myLines str
        | null word = []
        | otherwise = word : myLines rest
       where
        isNewline = flip elem "\n"
        trim = dropWhile isSpace str
        word = takeWhile (not . isNewline) trim
        rest = dropWhile (not . isNewline) trim
    ```

3. Now, let’s look at what those two functions have in common. Try writing a new function that parameterizes the character you’re breaking the string argument on and rewrite myWords and myLines using that parameter.

    ```haskell
    -- Parameterized version
    myChar :: Char -> String -> [String]
    myChar _ [] = []
    myChar charToBreakOn str
      | null word = []
      | otherwise = word : myChar charToBreakOn rest
     where
      isCharToBreakOn = flip elem [charToBreakOn]
      trim = dropWhile isSpace str
      word = takeWhile (not . isCharToBreakOn) trim
      rest = dropWhile (not . isCharToBreakOn) trim

    myWords :: String -> [String]
    myWords = myChar ' '

    myLines :: String -> [String]
    myLines = myChar '\n'
    ```

## Exercise: Comprehend thy lists

- `[x | x <- mySqr, rem x 2 == 0]` - Output would be all the square numbers, from 1 to 10, that are even (`[4,16,36,64,100]`)
- `[(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]` - Output would be a list of tuples (with an arity of 2), where both tuple elements are numbers within the set of squares from 1 to 10. However, for each tuple, the first number (`x`) is less than 50, while the second (`y`) is greater than 50 (`[(1,64),(1,81),(1,100),(4,64),(4,81),(4,100),(9,64),(9,81),(9,100),(16,64),(16,81),(16,100),(25,64),(25,81),(25,100),(36,64),(36,81),(36,100),(49,64),(49,81),(49,100)]`)
- `take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]` - Output is the same as the above, except limited to the first 5 elements of what was outputted above (`[(1,64),(1,81),(1,100),(4,64),(4,81)]`)

## Exercise: Square Cube

1. First write an expression that will make tuples of the outputs of `mySqr` and `myCube`.

    ```haskell
      mySqr = [x^2 | x <- [1..5]]
      myCube = [y^3 | y <- [1..5]]

      ans = [(x, y) | x <- mySqr, y <- myCube]
    ```

2. Now, alter that expression so that it only uses the x and y values that are less than 50.

    ```haskell
      mySqr = [x^2 | x <- [1..5]]
      myCube = [y^3 | y <- [1..5]]

      ans = [(x, y) | x <- mySqr, y <- myCube]
      ans2 = [(x, y) | (x, y) <- ans, x < 50 && y < 50]
    ```

3. Apply another function to that list comprehension to determine how many tuples inhabit your output list.

    ```haskell
      mySqr = [x^2 | x <- [1..5]]
      myCube = [y^3 | y <- [1..5]]

      ans = [(x, y) | x <- mySqr, y <- myCube]
      ans2 = [(x, y) | (x, y) <- ans, x < 50 && y < 50]
      ans3 = length ans2
    ```

## Exercise: Bottom Madness

1. `[x^y | x <- [1..5], y <- [2, undefined]]` will return bottom because list comprehension is strict on the values (and by extension the spine) of the input sets (lists)
2. `take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]` might not necessarily return bottom. However, I think it won't since, if we consider Haskell's laziness, only the first element of both input sets needs to be evaluated and neither is `undefined`. Will return `2`
3. `sum [1, undefined, 3]` will return bottom because `sum` is strict on the spine and on the values
4. `length [1, 2, undefined]` won't return bottom because, as we've learnt, `length` is strict only on the spine, not the values
5. `length $ [1, 2, 3] ++ undefined` will return bottom because the second list has an `undefined` spine which cannot be traversed by length. Moreover, it is possible that the application of `++` happens before the application of `length`.
6. `take 1 $ filter even [1, 2, 3, undefined]` won't return bottom because we only need to evaluate the first element of the filtered list. This can be done without reaching the `undefined` value at the end of the list. Will return `2`
7. `take 1 $ filter even [1, 3, undefined]` will return bottom because there isn't a preceding element that would return satisfying the `even` predicate, so `undefined` must be checked, and thus evaluated, then `bottom` is returned
8. `take 1 $ filter odd [1, 3, undefined]` won't return bottom for the same reason as number 6. Because of laziness we only require the first odd value in the list. The element `1` fits this description and thus, the `undefined` does not need to be evaluated. Returns `1`
9. `take 2 $ filter odd [1, 3, undefined]` won't return bottom because of laziness. We only need to evaluate up until we get the first 2 elements that are odd. `1` and `3` are odd, thus evaluation ceases after `3` and `undefined` is not evaluated. Will return `[1, 3]`
10. `take 3 $ filter odd [1, 3, undefined]` will return bottom because there is no third odd number in the list. The last element, `undefined`, will need to be checked (evaluated) causing the entire expression to return bottom

### Intermission: Is it in normal form?

1. `[1, 2, 3, 4, 5]` - NF
2. `1 : 2 : 3 : 4 : _` - NF, desugared version of #1
3. `enumFromTo 1 10` - Neither
4. `length [1, 2, 3, 4, 5]` - Neither
5. `sum (enumFromTo 1 10)` - Neither
6. `['a'..'m'] ++ ['n'..'z']` - Neither
7. `(_, 'b')` - WHNF

## Exercise: More bottoms

1. `take 1 $ map (+1) [undefined, 2, 3]` will return bottom because we are forcing the evaluation of the first element in the list which is `undefined`
2. `take 1 $ map (+1) [1, undefined, 3]` will not return bottom since the first element of the list is not `undefined`. Will return `[2]`
3. `take 2 $ map (+1) [1, undefined, 3]` will return bottom because we would be forcing the evaluation of the second element in the list as well as the first. The second element is `undefined`
4. The mystery function runs through each element of the list `xs` and checks whether each element is a vowel. The type is as follows: `itIsMystery :: String -> [Bool]`
5. `map (^2) [1..10]`: `[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]`
   1. `map minimum [[1..10], [10..20], [20..30]]`: `[1, 10, 20]`
   2. `map sum [[1..5], [1..5], [1..5]]`: `[15, 15, 15]`
6. `map (\x -> bool x (-x) (x == 3)) [1..10]`

## Exercises: Filtering

1. `filter (\x -> mod x 3 == 0) [1..30]`
2. `numOfMultiplesOfThree = length . filter (\x -> mod x 3 == 0)`
3. Filter articles `["the", "a", "an"]` from a sentence

    ```haskell
      articles :: [String]
      articles = ["the", "a", "an"]

      myFilter :: String -> [String]
      myFilter = filter (`notElem` articles) . words
    ```

## Zipping exercises

1. Write your own version of zip, and ensure it behaves the same as the original

    ```haskell
      myZip :: [a] -> [b] -> [(a, b)]
      myZip [] _= []
      myZip_ [] = []
      myZip (a : as) (b : bs) = (a, b) : myZip as bs
    ```

2. Do what you did for zip but now for zipWith

    ```haskell
    myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    myZipWith _ [] _ = []
    myZipWith _ _ [] = []
    myZipWith f (a : as) (b : bs) = f a b : myZipWith f as bs
    ```

3. Rewrite your zip in terms of the zipWith you wrote.

    ```haskell
    myZip' :: [a] -> [b] -> [(a, b)]
    myZip' = myZipWith (,)
    ```

## 9.12 Chapter Exercises

### Data.Char

1. Given the following functions (`isUpper`, `isLower`), which would we use to write a function that filters all the uppercase letters out of a `String`? Write that function such that, given the input "HbEfLrLxO", your function will return "HELLO".

    ```haskell
      filterUpper :: String -> String
      filterUpper = filter isUpper
    ```

2. Write a function that will capitalize the first letter of a string and return the entire string. For example, if given the argument "julie", it will return "Julie".

    ```haskell
      capitalize :: String -> String
      capitalize "" = ""
      capitalize (x : xs) = toUpper x : xs
    ```
