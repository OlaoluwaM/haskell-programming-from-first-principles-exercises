module Ch15.Semigroups where

import Test.QuickCheck hiding (Failure, Success)

import Data.Monoid

data Trivial = Trivial
    deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Arbitrary Trivial where
    arbitrary = pure Trivial

semigroupAssocProp :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssocProp a b c = a <> (b <> c) == (a <> b) <> c

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity a) <> (Identity b) = Identity $ a <> b

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = Two <$> arbitrary <*> arbitrary

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = Four <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj b) <> (BoolConj b') = BoolConj $ b && b'

instance Arbitrary BoolConj where
    arbitrary = BoolConj <$> arbitrary

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj b) <> (BoolDisj b') = BoolDisj $ b || b'

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

data Or a b = Fst a | Snd b deriving (Eq, Show)

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

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary

instance Show (Combine a b) where
    show _ = "Combine (a -> b)"

combineSemigroupAssocProp :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineSemigroupAssocProp f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

newtype Comp a = Comp {unComp :: a -> a}

instance (Semigroup a) => Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp $ f . g

instance (CoArbitrary a, Arbitrary a) => Arbitrary (Comp a) where
    arbitrary = Comp <$> arbitrary

instance Show (Comp a) where
    show _ = "Comp (a -> a)"

compSemigroupAssocProp :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssocProp f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Validation a b) where
    (Failure a) <> (Failure a') = Failure $ a <> a'
    suc@(Success _) <> (Success _) = suc
    _ <> suc@(Success _) = suc
    suc@(Success _) <> _ = suc

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, pure $ Failure a), (2, pure $ Success b)]

failure :: String -> Validation String Int
failure = Failure

success :: Int -> Validation String Int
success = Success

-- Move this to the test directory
main :: IO ()
main = do
    putStr "Trivial Semigroup Assoc Prop: "
    quickCheck (semigroupAssocProp @Trivial)

    putStr "Identity a Assoc Prop: "
    quickCheck (semigroupAssocProp @(Identity (Sum Int)))

    putStr "Identity a 2 Assoc Prop: "
    quickCheck (semigroupAssocProp @(Identity (Product Int)))

    putStr "Identity a 3 Assoc Prop: "
    quickCheck (semigroupAssocProp @(Identity (First (Maybe Int))))

    putStr "Two Assoc Prop: "
    quickCheck (semigroupAssocProp @(Two (Sum Int) String))

    putStr "Two Assoc Prop: "
    quickCheck (semigroupAssocProp @(Two (Sum Int) (Product Int)))

    putStr "Three Assoc Prop: "
    quickCheck (semigroupAssocProp @(Three Any (Sum Int) (Product Int)))

    putStr "Four Assoc Prop: "
    quickCheck (semigroupAssocProp @(Four (First (Maybe String)) (Last (Maybe Bool)) All (Product Int)))

    putStr "BoolConj Assoc Prop: "
    quickCheck (semigroupAssocProp @BoolConj)

    putStr "BoolDisj Assoc Prop: "
    quickCheck (semigroupAssocProp @BoolDisj)

    putStr "Or Assoc Prop: "
    quickCheck (semigroupAssocProp @(Or Int String))

    putStr "Combine Assoc Prop: "
    quickCheck (combineSemigroupAssocProp @(Sum Int) @Int)

    putStr "Comp Assoc Prop: "
    quickCheck (compSemigroupAssocProp @(Sum Int))

    putStrLn ""
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
