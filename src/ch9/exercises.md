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
