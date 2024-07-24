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
