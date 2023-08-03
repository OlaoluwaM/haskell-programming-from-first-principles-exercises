module Test where

bindExp :: Integer -> Integer
bindExp x =
  -- This errors out at `y` because `y` is not in scope
  let z = x + y
   in let y = 5
       in "the integer was: "
            ++ show x
            ++ " and y was: "
            ++ show y
            ++ " and z was: "
            ++ show z

bindExp' :: Integer -> String
bindExp' x =
  let x = 10; y = 5
   in "the integer was: " ++ show x ++ " and y was: " ++ show y
