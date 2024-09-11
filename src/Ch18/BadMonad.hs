{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch18.BadMonad where

import GHC.Generics (Generic)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Control.Applicative ((<**>))
import Data.Foldable (traverse_)

data CountMe a = CountMe Integer a deriving (Eq, Ord, Show, Generic, EqProp)

instance Functor CountMe where
    fmap f (CountMe i a) = CountMe i (f a)

instance Applicative CountMe where
    pure = CountMe 0
    CountMe n f <*> CountMe n' a = CountMe (n + n') (f a)

instance Monad CountMe where
    CountMe n a >>= f = let CountMe n' b = f a in CountMe (n + n') b

instance (Arbitrary a) => Arbitrary (CountMe a) where
    arbitrary = CountMe <$> arbitrary <*> arbitrary

main :: IO ()
main = do
    let trigger :: CountMe (Int, String, Int)
        trigger = undefined

    let testBatches = [trigger] <**> [functor, applicative, monad]
    traverse_ quickBatch testBatches
