module ChExercises where

  -- Exercise 8
  isPalindrome :: Eq a => [a] -> Bool
  isPalindrome xs = xs == reverse xs

  -- Exercise 9
  myAbs :: Integer -> Integer
  myAbs n = if n < 0 then n * (-1) else n

  -- Exercise 10
  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f tupleOne tupleTwo = ((snd tupleOne, snd tupleTwo), (fst tupleOne, fst tupleTwo))


  -- Correcting Syntax

  -- Exercise 1
  addOneToString :: String -> Int
  addOneToString xs = w + 1
                    where w = length xs

  -- Exercise 2
  id :: a -> a
  id x = x

  -- Exercise 3
  fTwo :: (Integer, Integer) -> Integer
  fTwo (a, b) = a
