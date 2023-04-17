{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

  example = 1

  oneA = (*9) 6

  oneB = head [(0, "doge"), (1, "kitten")]

  oneC = head [(0 :: Integer, "doge"), (1, "kitten")]

  oneD = if False then True else False

  oneE = length [1,2, 3, 4, 5]

  oneF = (length [1, 2, 3, 4]) > (length "TACOAT")

  two = w
    where
      w = y + 10;
      y = x + 5
      x = 5

  four = f
    where
        f = 4 / y
        y = x + 5
        x = 5

  five = f
    where
        f = x ++ y ++ z
        x = "Julie"
        y = " <3 "
        z = "Haskell"
