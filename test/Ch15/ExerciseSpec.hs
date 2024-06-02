module Ch15.ExerciseSpec where

import Ch15.Exercise
import Ch15.Playground

import Test.Hspec
import Test.Hspec.QuickCheck

import Data.Monoid

semigroupAssocProp :: (Eq a, Semigroup a) => a -> a -> a -> Bool
semigroupAssocProp a b c = a <> (b <> c) == (a <> b) <> c

compSemigroupAssocProp :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssocProp f g h a = unComp (f <> (g <> h)) a == unComp ((f <> g) <> h) a

combineSemigroupAssocProp :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineSemigroupAssocProp f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

monoidLawsProp :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidLawsProp a b c = semigroupAssocProp a b c && monoidLeftIdentityProp a && monoidRightIdentityProp a

spec :: Spec
spec = do
    describe "Semigroup Assoc prop tests for datatypes" $ do
        prop "Trivial" $ semigroupAssocProp @Trivial
        prop "Identity a" $ semigroupAssocProp @(Identity (Sum Int))
        prop "Identity a (2)" $ semigroupAssocProp @(Identity (Product Int))
        prop "Identity a (3)" $ semigroupAssocProp @(Identity (First (Maybe Int)))
        prop "Two a b" $ semigroupAssocProp @(Two (Sum Int) String)
        prop "Two a b (2)" $ semigroupAssocProp @(Two (Sum Int) (Product Int))
        prop "Three a b c" $ semigroupAssocProp @(Three Any (Sum Int) (Product Int))
        prop "Four a b c d" $ semigroupAssocProp @(Four (First (Maybe String)) (Last (Maybe Bool)) All (Product Int))
        prop "BoolConj" $ semigroupAssocProp @BoolConj
        prop "BoolDisj" $ semigroupAssocProp @BoolDisj
        prop "Or a b" $ semigroupAssocProp @(Or Int String)
        prop "Combine a b" $ combineSemigroupAssocProp @(Sum Int) @Int
        prop "Comp a" $ compSemigroupAssocProp @(Sum Int)

        it "Validation a" $
            and
                [ success 1 <> failure "blah" == Success 1
                , failure "woot" <> failure "blah" == Failure "wootblah"
                , success 1 <> success 2 == Success 1
                , failure "woot" <> success 2 == Success 2
                ]
    describe "Monoid Laws tests for datatypes" $ do
        prop "Trivial" $ monoidLawsProp @Trivial
        prop "Identity a" $ monoidLawsProp @(Identity (Sum Int))
        prop "Identity a (2)" $ monoidLawsProp @(Identity (Product Int))
        prop "Identity a (3)" $ monoidLawsProp @(Identity (First (Maybe Int)))
        prop "Two a b" $ monoidLawsProp @(Two (Sum Int) String)
        prop "Two a b (2)" $ monoidLawsProp @(Two (Sum Int) (Product Int))
        prop "Three a b c" $ monoidLawsProp @(Three Any (Sum Int) (Product Int))
        prop "Four a b c d" $ monoidLawsProp @(Four (First (Maybe String)) (Last (Maybe Bool)) All (Product Int))
        prop "BoolConj" $ monoidLawsProp @BoolConj
        prop "BoolDisj" $ monoidLawsProp @BoolDisj