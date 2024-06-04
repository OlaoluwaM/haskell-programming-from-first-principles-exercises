{-# LANGUAGE TupleSections #-}

module Ch15.Exercise where

import Test.QuickCheck hiding (Failure, Success)

import Data.Bifunctor (first)
import Data.Monoid

data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = pure Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity $ a <> b

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj b) <> (BoolConj b') = BoolConj $ b && b'

instance Monoid BoolConj where
    mempty = BoolConj $ getAll (mempty :: All)

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj b) <> (BoolDisj b') = BoolDisj $ b || b'

instance Monoid BoolDisj where
    mempty = BoolDisj $ getAny (mempty :: Any)

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

data Or a b = Fst a | Snd b deriving (Eq, Show)

-- This type cannot have a lawful Monoid instance because there is no valid `mempty` for the semigroup operation
instance Semigroup (Or a b) where
    b@(Snd _) <> _ = b
    _ <> b@(Snd _) = b
    _ <> b = b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, pure $ Fst a), (2, pure $ Snd b)]

newtype Combine a b = Combine {unCombine :: a -> b}

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine $ \n -> f n <> g n

instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine $ const mempty

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

instance Show (Combine a b) where
    show _ = "Combine (a -> b)"

newtype Comp a = Comp {unComp :: a -> a}

instance (Semigroup a) => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f . g

instance (Monoid a) => Monoid (Comp a) where
    mempty = Comp id

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = Comp <$> arbitrary

instance Show (Comp a) where
    show _ = "Comp (a -> a)"

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
    (Failure a) <> (Failure a') = Failure $ a <> a'
    suc@(Success _) <> (Success _) = suc
    _ <> suc@(Success _) = suc
    suc@(Success _) <> _ = suc

instance (Monoid a) => Monoid (Validation a b) where
    mempty = Failure mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, pure $ Failure a), (2, pure $ Success b)]

failure :: String -> Validation String Int
failure = Failure

success :: Int -> Validation String Int
success = Success

newtype Mem s a = Mem {runMem :: s -> (a, s)}

instance (Semigroup a) => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = Mem $ \s -> let (fa, fs) = f s in first (fa <>) (g fs)

instance (Monoid a) => Monoid (Mem s a) where
    mempty = Mem (mempty,)

instance (CoArbitrary s, Arbitrary a, Arbitrary s) => Arbitrary (Mem s a) where
    arbitrary = Mem <$> arbitrary

instance Show (Mem s a) where
    show _ = "Mem s -> (a, s)"

memTest :: IO ()
memTest = do
    let f' = Mem $ \s -> ("hi", s + 1)
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0

    print rmleft
    print rmright
    print (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
