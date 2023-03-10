module Exercise8 where

  -- Exercise 8
  isPalindrome :: Eq a => [a] -> Bool
  isPalindrome xs = xs == reverse xs

  -- Exercise 9
  myAbs :: Integer -> Integer
  myAbs n = if n < 0 then n * (-1) else n

  -- Exercise 10
  f :: (a, b) -> (c, d) -> ((b, d), (a, c))
  f tupleOne tupleTwo = ((snd tupleOne, snd tupleTwo), (fst tupleOne, fst tupleTwo))
