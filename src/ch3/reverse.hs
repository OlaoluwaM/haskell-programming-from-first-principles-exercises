module Reverse where

  rvrs :: String -> String
  rvrs str = drop 9 str ++ take 3 (drop 5 str) ++ " " ++ take 5 str

  main :: IO ()
  main = print $ rvrs "Curry is awesome"

  data Boole = True | False
