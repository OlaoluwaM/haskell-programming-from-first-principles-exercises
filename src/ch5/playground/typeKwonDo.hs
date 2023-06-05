module TypeKwonDo where

  -- Question 1
  f :: Int -> String
  f = undefined

  g :: String -> Char
  g = undefined

  h :: Int -> Char
  h = g . f

  -- Question 2
  data A
  data B
  data C

  q :: A -> B
  q = undefined

  w :: B -> C
  w = undefined

  e :: A -> C
  e = w . q

  -- Question 3
  data X
  data Y
  data Z

  xz :: X -> Z
  xz = undefined

  yz :: Y -> Z
  yz = undefined

  xform :: (X, Y) -> (Z, Z)
  xform (x, y) = (xz x, yz y)

  -- Question 4
  munge :: (x1 -> y1) -> (y1 -> (w1, z1)) -> x1 -> w1
  munge x1ToY1 y1ToTupleW1AndZ1 x1 = fst (y1ToTupleW1AndZ1 (x1ToY1 x1))
