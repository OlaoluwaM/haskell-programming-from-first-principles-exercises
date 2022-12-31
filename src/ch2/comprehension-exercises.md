# Comprehension Exercises

1. Done in GHCi REPL
2. The definition is as follows:

   ```haskell
      sqaure :: Int -> Int
      square x = x * x

      piNSquare :: Int -> Int
      piNSquare x = 3.14 * (square x)
   ```

3. The above definition using the constant `pi` instead of `3.14`:

   ```haskell
      piNSquare :: Int -> Int
      piNSquare x = pi * (square x)
   ```
