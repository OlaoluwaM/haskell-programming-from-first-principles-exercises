module Ch14.Trivial where

import Test.QuickCheck
import Test.QuickCheck (sample)

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen = return Trivial

instance Arbitrary Trivial where
  arbitrary = trivialGen

main :: IO ()
main = sample trivialGen
