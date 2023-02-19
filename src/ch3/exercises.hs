module Exercises where

  a :: String
  a = "Curry is awesome" ++ "!"

  exclaim :: String -> String
  exclaim str = str ++ "!"


  b :: Char
  b = (!!) "Curry is awesome" 4

  indexOfFourthElem :: String -> Char
  indexOfFourthElem str = (!!) str 4


  c :: String
  c = drop 9 "Curry is awesome!"

  dropNinthElem :: String -> String
  dropNinthElem = drop 9


  results :: [Bool]
  results = [a == "Curry is awesome!", b == 'y', c == "awesome!"]
