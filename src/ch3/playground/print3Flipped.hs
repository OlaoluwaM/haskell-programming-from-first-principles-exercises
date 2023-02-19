module Print3Flipped where

  myGreeting :: String
  myGreeting = (++) "Hello" " World"

  hello :: String
  hello = "Hello"

  world :: String
  world = "World"

  main :: IO ()
  main = do
    putStrLn myGreeting
    putStrLn secondGreeting
    where secondGreeting = (++) hello ((++) " " world)
