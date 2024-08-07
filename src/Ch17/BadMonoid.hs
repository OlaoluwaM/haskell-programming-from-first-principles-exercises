{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch17.BadMonoid where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import GHC.Generics

data Bull = Fools | Twoo deriving (Eq, Show, Generic, EqProp)

instance Arbitrary Bull where
    arbitrary = frequency [(1, pure Fools), (1, pure Twoo)]

instance Semigroup Bull where
    _ <> _ = Fools

instance Monoid Bull where
    mempty = Fools

main :: IO ()
main = quickBatch (monoid Twoo)
