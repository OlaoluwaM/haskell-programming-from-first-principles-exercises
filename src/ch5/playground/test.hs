module Test where
  import Data.Void

  -- y :: a -> b -> Bool
  y :: () -> Void
  y = undefined

  addStuff :: Integer -> Integer -> Integer
  addStuff a b = a + b + 5

  subtractStuff :: Integer -> Integer -> Integer
  subtractStuff x y = x - y - 10

  unCurried :: (Eq a, Num a) => (a, a) -> Bool
  unCurried (a, b) = a == b

  g = unCurried (5, 7)

  f :: a -> a
  f a = a

  hypothetical :: a -> (a -> a)
  hypothetical a b = b

  hypothetical' :: a -> (a -> a)
  hypothetical' a b = b

  anotherHypothetical :: a -> (b -> b)
  anotherHypothetical a b = b
