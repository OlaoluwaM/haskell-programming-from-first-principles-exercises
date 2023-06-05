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


  getThirdChar :: String -> Char
  getThirdChar str = str !! 2

  letterIndex :: Int -> Char
  letterIndex x = "Curry is awesome" !! x


  rvrs :: String -> String
  rvrs str = drop 9 str ++ take 3 (drop 5 str) ++ " " ++ take 5 str
