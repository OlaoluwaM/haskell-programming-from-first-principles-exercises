# Chapter 16 Exercises

## Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. (**Q**) What's the kind of `a`? | (**A**) `a` is of the kind `Type` in the type signature `f :: a -> a`
2. What are the kinds of `b` and `T`? (The `T` is capitalized on purpose!)

    In the type signature `f :: a -> b a -> T (b a)`, `b` inhabits the kind `Type -> Type`, while `T` is of kind `Type -> Type` as well

3. (**Q**) What's the kind of `c` | (**A**) `c` is of the kind `Type -> Type -> Type` in the type signature `f :: c a b -> c a b`

## Exercises: Heavy Lifting

Add `fmap`, parentheses, and function composition to each expression as needed for the expression to type check and produce the expected result. It may not always need to go in the same place, so don’t become complacent:

1. `a = (+1) $ read "[1]" :: [Int]`

    ```haskell
    a :: [Int]
    a = fmap (+ 1) (read "[1]" :: [Int])
    ```

2. `b = (++ "lol") (Just ["Hi,", "Hello"])`

    ```haskell
    b :: Maybe [String]
    b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello"])
    ```

3. `c = (*2) (\x -> x - 2)`

    ```haskell
    c :: Integer -> Integer
    c = fmap (* 2) (\x -> x - 2)
    ```

4. `d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])`

    ```haskell
    d :: Integer -> [Char]
    d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])
    ```

5. `e :: IO Integer`

    ```haskell
    e :: IO Integer
    e = let ioi = readIO "1" :: IO Integer
            changed = read ("123"++) show ioi
        in (*3) changed

    -- Answer
    e :: IO Integer
    e = let ioi = readIO "1" :: IO Integer in fmap ((* 3) . read . ("123" ++) . show) ioi
    ```
