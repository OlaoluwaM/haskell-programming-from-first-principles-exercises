module Ch14.Addition where

import Ch8.Playground (dividedBy, recursiveMult)
import Test.Hspec

import Test.QuickCheck

test :: IO ()
test = hspec $ do
  describe "Division" $ do
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` Just (5, 0)
    it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` Just (4, 2)
  describe "Addition" $ do
    it "should ensure that x + 1 is always greater than x" $ do
      property $ \x -> (x + 1) > (x :: Integer)

test2 :: IO ()
test2 = hspec $ do
  describe "Recursive Multiplication" $ do
    it "should check that 5 can be recursively multiplied with 5 to give 25" $ do
      recursiveMult 5 5 `shouldBe` 25
    it "should check that 9 can be recursively multiplied with 15 to give 135" $ do
      recursiveMult 9 15 `shouldBe` 135

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  (a, b) <- genTuple
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  (a, b) <- genTuple
  elements [Left a, Right b]

genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Just a, Nothing]

genMaybe' :: (Arbitrary a) => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return $ Just a)]

propAdditionGreater :: Int -> Bool
propAdditionGreater x = x + 1 > x

propAdditionGreaterUntrue :: Int -> Bool
propAdditionGreaterUntrue x = x + 0 > x

runQc :: IO ()
runQc = quickCheck propAdditionGreater

runQcFail :: IO ()
runQcFail = quickCheck propAdditionGreaterUntrue
