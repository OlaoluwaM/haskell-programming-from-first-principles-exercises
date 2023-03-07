module GreetIfCool where

  greetIfCool :: String -> IO ()
  greetIfCool coolness =
    if cool coolness
      then putStrLn "Cool"
    else
      putStrLn "Nope"
    where cool v =
            v == "Cool"
