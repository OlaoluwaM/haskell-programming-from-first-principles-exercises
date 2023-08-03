module BagGrab where

-- 1
-- These are all equivalent

mTh :: (Num a) => a -> a -> a -> a
mTh x y z = x * y * z

mTh' x y = \z -> x * y * z

mTh'' x = \y -> \z -> x * y * z

mTh''' = \x -> \y -> \z -> x * y * z

-- 2
-- `a` in a source file, `d` in GHCi

mTh3 = mTh 3

d = 3

-- 3 a
addOneIfOdd n = if odd n then f n else n
  where
    f n = n + 1

-- 3 b
addFive = \x -> \y -> min x y + 5

-- 3 c
mflip f x y = f y x
