module Ch15.Playground where

import Debug.Trace (traceIO)
import Test.QuickCheck (Arbitrary (arbitrary), frequency, quickCheck)

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Nada <> a@(Only _) = a
  a@(Only _) <> Nada = a
  (Only a) <> (Only b) = Only $ a <> b

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada

-- Madlibs

newtype Adjective = Adjective String deriving (Eq, Show)
newtype Adverb = Adverb String deriving (Eq, Show)
newtype Noun = Noun String deriving (Eq, Show)
newtype Exclamation = Exclamation String deriving (Eq, Show)

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' (Exclamation e) (Adverb adv) (Noun noun) (Adjective adj) = e <> " ! he said " <> adv <> " as he jumped into his car " <> noun <> " and drove off with his " <> adj <> " wife. "

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' (Exclamation e) (Adverb adv) (Noun noun) (Adjective adj) = mconcat [e, " ! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife. "]

monoidAssocProp :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssocProp a b c = a <> (b <> c) == (a <> b) <> c

monoidLeftIdentityProp :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentityProp m = mempty <> m == m

monoidRightIdentityProp :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentityProp m = m <> mempty == m

newtype First' a = First' {getFirst' :: Optional a} deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [(3, pure $ First' $ Only a), (1, pure $ First' Nada)]

instance Semigroup (First' a) where
  a@(First' (Only _)) <> _ = a
  _ <> a@(First' (Only _)) = a
  _ <> _ = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

main :: IO ()
main = do
  traceIO "Monoid Assoc prop"
  quickCheck (monoidAssocProp @(First' String))
  traceIO "Monoid Identity props"
  mapM_ quickCheck [monoidLeftIdentityProp @(First' String), monoidRightIdentityProp @(First' String)]
