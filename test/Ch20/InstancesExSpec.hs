module Ch20.InstancesExSpec where

import Test.Hspec

import Ch20.InstancesEx

import Data.Bifunctor (first)
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = describe "Using the checkers package to validate Foldable instances (Foldable laws)" $ do
    testBatch (first (const "Constant a b") $ foldable exampleConstant)
    testBatch (first (const "Two a b") $ foldable exampleTwo)
    testBatch (first (const "Three a b c") $ foldable exampleThree)
    testBatch (first (const "Three' a b") $ foldable exampleThree')
    testBatch (first (const "Four' a b") $ foldable exampleFour')
