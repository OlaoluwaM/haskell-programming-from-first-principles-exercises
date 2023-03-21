# Chapter Exercises

1. `length :: [a] -> Integer`
   1. In GHCi, the type of the `length` function is `Foldable t => t a -> Int`.
2. The length of the following
   1. `length [1, 2, 3, 4, 5] == 5`
   2. `length [(1, 2), (2, 3), (3, 4)] == 3`
   3. `length allAwesome == 2`
   4. `length (concat allAwesome) == 5`
3. `div 6 (length [1, 2, 3])` (4)
4. Type: `Bool`, Result: `True` (5)
5. For the following (6)
   1. Type: `Num a => a`, Result: `5`
   2. Type: `Bool`, Result: `False`
6. Answers: (7)
   1. Works because were comparing two `Int` values, `length allAwesome` and `2 :: Int`. Reduces to `True`
   2. Doesn't work because list must be homogenous, which `[1, 'a', 3, 'b']` is not
   3. Works because `allAwesome` and `awesome` are members of the `Foldable` type class, thus can be called with the `length` function. Reduces to `5`
   4. Works because both expressions, `(8 == 8)` and `('b' < 'a'>)` reduce to expression, `True` and `False` respectively. Reduces to expression `False`
   5. Won't work because `9` is not a valid operand to the boolean conjunction expressions

## Match function names to their types

1. c
2. b
3. a
4. d


