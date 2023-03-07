module GreetIfCool where

  greetIfCool :: String -> IO ()
  greetIfCool coolness =
    if cool
      then putStrLn "Cool"
    else
      putStrLn "Nope"
    where cool =
            coolness == "Cool"
