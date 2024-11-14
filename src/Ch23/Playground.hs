module Ch23.Playground where

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5== 0 = "Buzz"
  | n `mod` 3== 0 = "Fizz"
  | otherwise= show n

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = map fizzBuzz [from..to]

main :: IO ()
main = mapM_ putStrLn (fizzbuzzFromTo 1 100)
