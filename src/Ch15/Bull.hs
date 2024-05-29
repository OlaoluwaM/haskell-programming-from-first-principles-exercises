module Ch15.Bull where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

import Ch15.Playground

data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary = oneof [pure Fools, pure Twoo]

instance Semigroup Bull where
  _ <> _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

main :: IO ()
main = do
  let idProps = [monoidLeftIdentityProp @Bull, monoidRightIdentityProp @Bull]
  quickCheck (monoidAssocProp @Bull)
  mapM_ quickCheck idProps
