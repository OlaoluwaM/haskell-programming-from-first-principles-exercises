# Chapter 8 Exercises

## Intermission: Exercise

```haskell
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes (n-1) f b)

applyTimes 5 (+1) 5
{-
(+1) ((+1) ((+1) ((+1) ((+1) 5))))
(+1) ((+1) ((+1) ((+1) 6)))
(+1) ((+1) ((+1) 7))
(+1) ((+1) 8)
(+1) 9
10
-}
```

## 8.6 Chapter Exercises

### Review of types

1. What is the type of `[[True, False], [True, True], [False, True]]`

    ```haskell
      -- d
      item :: [[Bool]]
      item = [[True, False], [True, True], [False, True]]
    ```

2. Which of the following has the same type as `[[True, False], [True, True], [False, True]]`

    B, `[[3 == 3], [6 > 5], [3 < 4]]`

3. For the function below, which of the following statements are true

    ```haskell
      func :: [a] -> [a] -> [a]
      func x y = x ++ y
    ```

    D, all the above

4. For the func code above, which is a valid application of func to both of its arguments

    B, `func "Hello" "World"`

### Reviewing currying

Given the following definitions, tell us what value results from further applications:

```haskell
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"
```

1. What is the value of appedCatty "woohoo!"? Try to determine the answer for yourself, then test it in the REPL: `"woops mrow woohoo"`
2. `frappe "1"` evaluates to `"1 mrow haha"`
3. `frappe (appedCatty "2")` evaluates to `"woops mrow 2 mrow haha"`
4. `appedCatty (frappe "blue")` evaluates to `"woops mrow blue mrow haha"`
5. `cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue"))` evaluates to `"pink mrow haha mrow green mrow woops mrow blue"`
6. `cattyConny (flippy "Pugs" "are") "awesome"` evaluates to `"are mrow Pugs mrow awesome"`

### Recursion

```haskell
type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

dividedBy :: Numerator -> Denominator -> (Quotient, Remainder)
dividedBy n d = go n d 0
 where
  go :: Numerator -> Denominator -> Integer -> (Quotient, Remainder)
  go n' 1  _ = (n', 0)
  go n' d' c = if (n' < d' || n' == 0) then (c, n') else go (n' - d') d' (c + 1)

main :: IO ()
main = do
  print $ dividedBy 25 4
  print $ dividedBy 4 25
  print $ dividedBy 20 4
  print $ dividedBy 20 1
```

1. Write out the steps for reducing `dividedBy 15 2` to its final answer according to the Haskell code.

    ```text
    dividedBy 15 2
    go 15 2 0
    go (15 - 2) 2 1
    go (13 - 2) 2 2
    go (11 - 2) 2 3
    go (9 - 2) 2 4
    go (7 - 2) 2 5
    go (5 - 2) 2 6
    go (3 - 2) 2 7
    (7, 1)
    ```

2. Write a function that recursively sums all numbers from 1 to $n$, $n$ being the argument. So if $n$ is 5, you’d add $1 + 2 + 3 + 4 + 5$ to get 15. The type should be `(Eq a, Num a) => a -> a`.

    ```haskell
    recursiveSum :: (Eq a, Num a) => a -> a
    recursiveSum n = go n n
     where
      go 0 summation = summation
      go n summation = let predecessor = n - 1 in go predecessor (summation + predecessor)
    ```

3. Write a function that multiplies two integral numbers using recursive summation. The type should be `(Integral a) => a -> a -> a`.

    ```haskell
    recursiveMult :: (Integral a) => a -> a -> a
    recursiveMult _ 0 = 0
    recursiveMult n multiplier = go n multiplier 0
     where
       go _ 0 output = output
       go n multiplier output = go n (multiplier - 1) (output + n)
    ```

### Fixing `dividedBy`

```haskell
import GHC.Num (integerIsNegative)

type Numerator = Integer
type Denominator = Integer
type Quotient = Integer
type Remainder = Integer

dividedBy :: Numerator -> Denominator -> Maybe (Quotient, Remainder)
dividedBy n 0 = Nothing
dividedBy n d
  | integerIsNegative n && integerIsNegative d = Just $ go (abs n) (abs d) 0
  | integerIsNegative n || integerIsNegative d = Just $ fstMap (* (-1)) (go (abs n) (abs d) 0)
  | otherwise = Just $ go n d 0
 where
  go :: Numerator -> Denominator -> Integer -> (Quotient, Remainder)
  go n' 1 _ = (n', 0)
  go n' d' c = if n' < d' || n' == 0 then (c, n') else go (n' - d') d' (c + 1)

fstMap :: (Num a) => (a -> a) -> (a, a) -> (a, a)
fstMap f (x, y) = (f x, y)
```

### McCarthy 91 function

Convert the following notation to Haskell

$$
\begin{aligned}
  MC(n) =
  \begin{cases}
    \,n - 1 & if\,n\,>\,100 \\
    \,MC(MC(n\,+\,11)) & if\,n\,\leq\,100
  \end{cases}
\end{aligned}
$$

```haskell
mc91 :: Integer -> Integer
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

main :: IO ()
main = do
  let expectedResult = [91,91,91,91,91,91,91,92,93,94,95,96,97,98,99,100]
  let output = map mc91 [95..110]
  print $ output == expectedResult
```

### Number to Words

```haskell
import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "not a single digit" -- For any other input

digits :: Int -> [Int]
digits n = go n []
 where
  go :: Int -> [Int] -> [Int]
  go n' digits'
    | n' < 10 = n' : digits'
    | otherwise = go (div n' 10) (mod n' 10 : digits')

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

main :: IO ()
main = do
  let expectedOutput = "one-two-three-two-four-five-four-six"
  let output = wordNumber 12324546
  print $ output == expectedOutput
```
