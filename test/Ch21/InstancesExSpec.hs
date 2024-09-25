module Ch21.InstancesExSpec where

import Test.Hspec

import Ch21.InstancesEx

import Data.Bifunctor (first)
import Data.Monoid
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = describe "Using the checkers package to validate Traversable instances (Traversable laws)" $ do
    testBatch (first (const "Identity a (Functor)") $ functor (undefined :: Identity (All, Int, String)))
    testBatch (first (const "Identity a (Foldable)") $ foldable (undefined :: Identity (String, Bool, Product Int, Int, Int)))
    testBatch (first (const "Identity a (Traversable)") $ traversable (undefined :: Identity (Maybe Int, [Int], [Int], Sum Int)))

    testBatch (first (const "Constant a b (Functor)") $ functor (undefined :: Constant () (Any, Integer, String)))
    testBatch (first (const "Constant a b (Foldable)") $ foldable (undefined :: Constant () (String, Int, Identity String, Int, Integer)))
    testBatch (first (const "Constant a b (Traversable)") $ traversable (undefined :: Constant () (Maybe Bool, [String], [Char], Product Int)))

    testBatch (first (const "Optional a (Functor)") $ functor (undefined :: Optional (String, Bool, String)))
    testBatch (first (const "Optional a (Foldable)") $ foldable (undefined :: Optional (String, Bool, Sum Int, Integer, String)))
    testBatch (first (const "Optional a (Traversable)") $ traversable (undefined :: Optional (Maybe Bool, [Int], [String], Any)))

    testBatch (first (const "List a (Functor)") $ functor (undefined :: List (All, Int, String)))
    testBatch (first (const "List a (Foldable)") $ foldable (undefined :: List (Int, Bool, Any, Int, Bool)))
    -- testBatch (first (const "List a (Traversable)") $ traversable (undefined :: List (Identity Int, [Int], [String], Sum Int))) Too slow

    testBatch (first (const "Three a b c (Functor)") $ functor (undefined :: Three String Bool (All, String, Int)))
    testBatch (first (const "Three a b c (Foldable)") $ foldable (undefined :: Three Int Int (String, Bool, Sum Integer, Integer, Bool)))
    testBatch (first (const "Three a b c (Traversable)") $ traversable (undefined :: Three Int Int (Optional Int, [String], [Int], Product Integer)))

    testBatch (first (const "Pair a b (Functor)") $ functor (undefined :: Pair () (Any, Int, Bool)))
    testBatch (first (const "Pair a b (Foldable)") $ foldable (undefined :: Pair () (String, String, Product Int, Int, Bool)))
    testBatch (first (const "Pair a b (Traversable)") $ traversable (undefined :: Pair () (Three All String Int, [String], [Int], Product Integer)))

    testBatch (first (const "Big a b (Functor)") $ functor (undefined :: Big () (String, Sum Int, Integer)))
    testBatch (first (const "Big a b (Foldable)") $ foldable (undefined :: Big () (String, Bool, All, Int, Int)))
    testBatch (first (const "Big a b (Traversable)") $ traversable (undefined :: Big () (Pair All Int, [String], [Int], Product Integer)))

    testBatch (first (const "Bigger a b (Functor)") $ functor (undefined :: Bigger () (Int, All, String)))
    testBatch (first (const "Bigger a b (Foldable)") $ foldable (undefined :: Bigger () (String, Int, All, Integer, Int)))
    testBatch (first (const "Bigger a b (Traversable)") $ traversable (undefined :: Bigger () (Pair All Int, [String], [Int], Product Integer)))

    testBatch (first (const "S n a (Functor)") $ functor (undefined :: S (Either String) (String, Integer, Bool)))
    testBatch (first (const "S n a (Foldable)") $ foldable (undefined :: S Maybe (String, Int, Any, Integer, Int)))
    testBatch (first (const "S n a (Traversable)") $ traversable (undefined :: S Optional (Big All Int, [String], [Int], Product Integer)))

    testBatch (first (const "Tree a (Functor)") $ functor (undefined :: Tree (String, Int, Bool)))
    testBatch (first (const "Tree a (Foldable)") $ foldable (undefined :: Tree (String, Bool, Sum Int, Int, Int)))

-- testBatch (first (const "Tree a (Traversable)") $ traversable (undefined :: Tree ([Bool], [Bool], Bool, All))) Too slow
