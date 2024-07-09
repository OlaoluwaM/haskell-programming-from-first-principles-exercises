module Ch16.ExFuncInstancesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Ch16.ExFuncInstances

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g . fmap f) x == fmap (g . f) x

spec :: Spec
spec = describe "Using QuickCheck to validate Functor instances" $ do
    prop "Identity a" $ \ida -> functorIdentity ida && functorCompose (+ 1) (* 2) (ida :: Identity Int)
