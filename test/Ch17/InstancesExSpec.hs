module Ch17.InstancesExSpec where

import Test.Hspec

import Ch17.InstancesEx

import Data.Bifunctor (first)
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

-- TODO also include tests for the functor laws for each type
spec :: Spec
spec = describe "Using the checkers package to validate Applicative instances (applicative laws)" $ do
    testBatch (first (const "Pair a") $ applicative examplePair)
    testBatch (first (const "Two a b") $ applicative exampleTwo)
    testBatch (first (const "Three a b c") $ applicative exampleThree)
    testBatch (first (const "Three' a b") $ applicative exampleThree')
    testBatch (first (const "Four a b c d") $ applicative exampleFour)
    testBatch (first (const "Four' a b") $ applicative exampleFour')
