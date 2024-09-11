# Chapter 17 Exercises

## Exercises: Lookups

Make the following expressions type check using: `pure`, `(<$>)`, and `<*>`

1. `added`

    ```haskell
    added :: Maybe Integer
    added = (+3) <$> lookup 3 (zip [1..3] [4..6])
    ```

2. `tupled`

    ```haskell
    y :: Maybe Integer
    y  = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

    z :: Maybe Integer
    z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

    tupled :: Maybe (Integer, Integer)
    tupled = (,) <$> y <*> z
    ```

3. `maxed`

    ```haskell
    x :: Maybe Int
    x = L.elemIndex 3 [1..5]

    y' :: Maybe Int
    y' = L.elemIndex 4 [1..5]

    max' :: Int -> Int -> Int
    max' = max

    maxed :: Maybe Int
    maxed = max' <$> x <*> y'
    ```

4. `summed`

    ```haskell
    xs :: [Integer]
    xs = [1..3]

    ys :: [Integer]
    ys = [4..6]

    x' :: Maybe Integer
    x' = lookup 3 $ zip xs ys

    y'' :: Maybe Integer
    y'' = lookup 2 $ zip xs ys

    summed :: Maybe Integer
    summed = sum <$> ((,) <$> x' <*> y'')
    ```

## Exercise: Identity instance

```haskell
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity

    (Identity f) <*> (Identity a) = Identity (f a)

-- Equivalent to the above Applicative instance perhaps because `Identity` has no effects
-- instance Applicative Identity where
--     pure = Identity

--     (Identity f) <*> idA = fmap f idA
```

## Exercise: Constant Instance

```haskell
newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Show, Ord)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (Constant a) <*> (Constant b) = Constant (a <> b)
```

## Exercise: Fixer upper

1. `const <$> Just "Hello" <*> "World"`

    ```haskell
    const <$> Just "Hello" <*> "World"
    ```

2. `(,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]`

    ```haskell
    (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
    ```

## Exercise: Variations on Either

```haskell
data Validation e a = Failure e | Success a deriving (Eq, Show)

instance Functor (Validation e) where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)

instance (Monoid e) => Applicative (Validation e) where
    pure = Success

    (Failure e) <*> (Failure e') = Failure (e <> e')
    (Failure e) <*> _ = Failure e
    _ <*> (Failure e) = Failure e
    (Success f) <*> (Success a) = Success (f a)
```

## Section 17.17

Given a type that has an instance of Applicative, specialize the types of the methods. Test your specialization in the REPL. One way to do this is to bind aliases of the type class methods to more concrete types that have the type we tell you to fill in

1. `[]`

    ```haskell
    -- type is []
    pure :: a -> [a]
    (<*>) :: [(a -> b)] -> [a] -> [b]
    ```

2. `IO`

    ```haskell
    -- type is IO
    pure :: a -> IO a
    (<*>) :: IO (a -> b) -> IO a -> IO b
    ```

3. `(,) a`

    ```haskell
    -- type is (a,) or (,) a
    pure :: b -> (a, b)
    (<*>) :: (a, b -> c) -> (a, b) -> (a, c)
    ```

4. `(->) e`

    ```haskell
    -- type if (e -> ) or (->) e
    pure :: a -> (e -> a)
    (<*>) :: (e -> (a -> b)) -> (e -> a) -> (e -> b)
    ```

Write instances for the following datatypes. Confused? Write out what the types should be. Use the checkers library to validate the instances:

[Instance definitions can be found here](InstancesEx.hs), and the code checking the lawfulness of those definition is [here](../../test/Ch17/InstancesExSpec.hs)

### Combinations

Remember the vowels and stops exercise from Chapter 10, on folds? Write a function to generate all the possible combinations of three input lists, using liftA3 from Control.Applicative:

```haskell
import Control.Applicative (liftA3)

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (,,)

combos' :: [(Char, Char, Char)]
combos' = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

combos stops vowels stops == combos' -- True
```
