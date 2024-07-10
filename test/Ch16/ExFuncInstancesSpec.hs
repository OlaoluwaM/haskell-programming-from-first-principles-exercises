module Ch16.ExFuncInstancesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import Ch16.ExFuncInstances

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g . fmap f) x == fmap (g . f) x

spec :: Spec
spec = describe "Using QuickCheck to validate Functor instances (check functor laws)" $ do
    prop "Identity a" $ \ida -> functorIdentity ida && functorCompose (+ 1) (* 2) (ida :: Identity Int)

    prop "Pair a" $ \pair -> functorIdentity pair && functorCompose (+ 1) (* 2) (pair :: Pair Int)

    prop "Two a b" $ \twoAB -> functorIdentity twoAB && functorCompose (++ "World") (++ "Hi") (twoAB :: Two Int String)

    prop "Three a b c" $ \threeABC -> functorIdentity threeABC && functorCompose (True ||) not (threeABC :: Three String Int Bool)

    prop "Three' a b" $ \threeAB' -> functorIdentity threeAB' && functorCompose (True ||) not (threeAB' :: Three' String Bool)

    prop "Four a b c d" $ \fourABCD -> functorIdentity fourABCD && functorCompose (subtract 9) (* 12) (fourABCD :: Four String Int Bool Integer)

    prop "Four' a b" $ \fourAB' -> functorIdentity fourAB' && functorCompose (subtract 9) (* 12) (fourAB' :: Four' String Int)
