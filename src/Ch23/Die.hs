module Ch23.Die where

import Control.Applicative (liftA3)
import Control.Lens.Combinators
import Control.Monad (replicateM, unless)
import Control.Monad.Trans.State
import Data.Bifunctor (bimap, second)
import System.Random

data Die = DieOne | DieTwo | DieThree | DieFour | DieFive | DieSix deriving (Eq, Show, Ord, Enum)

rollDie :: State StdGen Die
rollDie = toEnum . subtract 1 <$> state (randomR (1, 6))

rollDieThreeTimes :: State StdGen (Die, Die, Die)
rollDieThreeTimes = liftA3 (,,) rollDie rollDie rollDie

infiniteRoll :: Int -> State StdGen [Die]
infiniteRoll n = replicateM n rollDie

rollsToGetTwenty' :: StdGen -> Int
rollsToGetTwenty' = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= 20 = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
        | sum >= n = count
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n = go 0 0 []
  where
    go :: Int -> Int -> [Int] -> StdGen -> (Int, [Die])
    go sum count diesRolled gen
        | sum >= n = (count, map toEnum diesRolled)
        | otherwise =
            let (die, nextGen) = randomR (1, 6) gen
             in go (sum + die) (count + 1) (die : diesRolled) nextGen

rollsToGetToTwenty :: State (StdGen, (Int, Int, [Die])) (Int, [Die])
rollsToGetToTwenty = do
    dieRoll <- zoom _1 rollDie
    let dieInt = fromEnum dieRoll
    modify (\(seed, (rollSum, count, rolls)) -> (seed, (rollSum + dieInt, count + 1, dieRoll : rolls)))
    (resultantSum, count, allRolls) <- gets snd
    if resultantSum >= 20 then pure (count, allRolls) else rollsToGetToTwenty

rollsToGetToN :: Int -> State (StdGen, (Int, Int, [Die])) (Int, [Die])
rollsToGetToN n = do
    dieRoll <- zoom _1 rollDie
    let dieInt = fromEnum dieRoll
    modify (\(seed, (rollSum, count, rolls)) -> (seed, (rollSum + dieInt, count + 1, dieRoll : rolls)))
    (resultantSum, count, allRolls) <- gets snd
    if resultantSum >= n then pure (count, reverse allRolls) else rollsToGetToTwenty
