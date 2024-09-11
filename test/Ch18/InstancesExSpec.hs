module Ch18.InstancesExSpec where

import Test.Hspec

import Ch18.InstancesEx

import Data.Bifunctor (first)
import Test.Hspec.Checkers
import Test.QuickCheck.Classes

spec :: Spec
spec = describe "Using the checkers package to validate Applicative instances (applicative laws)" $ do
    testBatch (first (const "Nope a") $ monad exampleNope)
    testBatch (first (const "BahEither b a") $ monad exampleBahEither)
    testBatch (first (const "Identity a") $ monad exampleIdentity)
    testBatch (first (const "List a") $ monad exampleList)
