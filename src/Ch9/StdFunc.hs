module StdFunc where

-- 1. `myOr` returns `True` if any Bool in the list is `True`
myOr :: [Bool] -> Bool
myOr = foldr (||) False

myOrRecursive :: [Bool] -> Bool
myOrRecursive [] = False
myOrRecursive (b : bs) = b || myOrRecursive bs

-- 2. `myAny` returns `True` if `a -> Bool` applied to any of the values in the list returns `True`:
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _ [] = False
myAny' f (a : as) = f a || myAny' f as

-- 3. After you write the recursive `myElem`, write another version that uses `any`. The built-in version of `elem` in GHC 7.10 and newer has a type that uses `Foldable` instead of the list type, specifically. You can ignore that and write the concrete version that works only for lists:
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a' (a : as) = a == a' || myElem a' as

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' a = any (== a)

-- 4. Implement `myReverse`
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (a : as) = myReverse' as ++ [a]

myReverse' :: [a] -> [a]
myReverse' = foldl (flip (:)) []

-- 5. `squish` flattens a list of lists into a list
squish :: [[a]] -> [a]
squish [] = []
squish (innerList : otherLists) = innerList ++ squish otherLists

squishAlt :: [[a]] -> [a]
squishAlt otherLists = foldr (++) [] otherLists

-- 6. `squishMap` maps a function over a list and concatenates the results:
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (a : as) = f a ++ squishMap f as

-- 7. `squishAgain` flattens a list of lists into a list. This time, re-use the `squishMap` function:
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 8. `myMaximumBy` takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returns GT for. If you import maximumBy from Data.List, you’ll see that the type is
{-
  The myMaximumBy function determines the maximum of a list by recursively comparing each element with the maximum value of the remaining elements. That is the relationship it leverages to find the maximum value. The maximum value of a list is the greater value between the head of the list and the maximum of the tail of the list.

  In the base case, when the list contains only one element, this element is the maximum by default.

  For longer lists, the function first finds the maximum of the rest of the list (all elements except the first one). After obtaining this value, it compares that "tail maximum" with the current element (which can also be considered the head of the list at that point in time) using a user-defined comparison function `f`. Depending on the result of this comparison — whether the current element (AKA the head) is greater, equal, or less than the "tail maximum" — the function decides which element to return as the maximum at that step/point. This process continues until the entire list is evaluated, and the overall maximum is determined
-}
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [a] = a
myMaximumBy f (a : as) = case f a tailMaximum of
  GT -> a
  _ -> tailMaximum
 where
  tailMaximum = myMaximumBy f as

-- 9. `myMinimumBy` takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returns LT for
-- Function the same as `myMaximumBy` so the explanation for that function still holds in this case. Only difference here is that minimum is our focus, not the maximum
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [a] = a
myMinimumBy f (a : as) = case f a tailMinimum of
  LT -> a
  _ -> tailMinimum
 where
  tailMinimum = myMaximumBy f as

-- 10. Using the myMinimumBy and myMaximumBy functions, write your own versions of maximum and minimum. If you have GHC 7.10 or newer, you’ll see a type constructor that wants a Foldable instance instead of a list, as has been the case for many functions so far
type FnType a = (Ord a) => [a] -> a

myMaximum :: FnType a
myMaximum = myMaximumBy compare

myMinimum :: FnType a
myMinimum = myMinimumBy compare
