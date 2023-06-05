module TypeC where

  class NumPrime a where
    fromInt :: Int -> a
    (+/) :: a -> a -> a

  sum' :: NumPrime a => [a] -> a
  sum' = foldr (+/) (fromInt 0)

  instance NumPrime Int where
    fromInt n = n
    (+/) = (Prelude.+)

  printIncr :: (Show a, NumPrime a) => a -> IO ()
  printIncr x = print (x +/ fromInt 1)

  class (Eq a, Num a, NumPrime a) => Mul a where
    (*/) :: a -> a -> a
    x */ _ | x == fromInt 0 = fromInt 0
    _ */ y | y == fromInt 0 = fromInt 0
    x */ y | x == fromInt 1 = y
    x */ y | y == fromInt 1 = x
    x */ y = x +/ (x */ (y - 1))

  instance Mul Int where
    (*/) = (Prelude.*)

  printNested :: Show a => Int -> a -> IO ()
  printNested 0 x = print x
  printNested n x = printNested (n - 1) (replicate n x)
