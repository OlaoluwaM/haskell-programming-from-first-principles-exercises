{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Ch20.InstancesEx where

import Data.Monoid
import GHC.Generics
import Test.QuickCheck (Arbitrary (arbitrary))
import Test.QuickCheck.Checkers (EqProp)

newtype Constant a b = Constant b deriving (Eq, Show, Ord, Generic, EqProp)

instance Foldable (Constant a) where
    foldr f startingVal (Constant b) = f b startingVal

instance (Arbitrary b) => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

exampleConstant :: Constant () (Int, Bool, Sum Integer, Product Integer, Integer)
exampleConstant = undefined

data Two a b = Two a b deriving (Eq, Show, Ord, Generic, EqProp)

-- The 'a' here is part of the structure so remains unchanged
instance Foldable (Two a) where
    foldr f startingVal (Two _ b) = f b startingVal

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

exampleTwo :: Two Bool (Int, Bool, Sum Integer, Product Integer, Integer)
exampleTwo = undefined

data Three a b c = Three a b c deriving (Eq, Show, Ord, Generic, EqProp)

instance Foldable (Three a b) where
    foldr f startingVal (Three _ _ c) = f c startingVal

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

exampleThree :: Three String Int (Int, Bool, Sum Integer, Product Integer, Integer)
exampleThree = undefined

data Three' a b = Three' a b b deriving (Eq, Show, Ord, Generic, EqProp)

instance Foldable (Three' a) where
    foldr f startingVal (Three' _ b b') = f b (f b' startingVal)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = Three' <$> arbitrary <*> arbitrary <*> arbitrary

exampleThree' :: Three' Integer (Int, Bool, Sum Integer, Product Integer, Integer)
exampleThree' = undefined

data Four' a b = Four' a b b b deriving (Eq, Show, Ord, Generic, EqProp)

instance Foldable (Four' a) where
    foldr f startingVal (Four' _ b b' b'') = f b (f b' (f b'' startingVal))

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = Four' <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

exampleFour' :: Four' (Int, Bool) (Int, Bool, Sum Integer, Product Integer, Integer)
exampleFour' = undefined
