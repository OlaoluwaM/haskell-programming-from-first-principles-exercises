-- This is a type signature
sayHello :: String -> IO ()

-- This is the runtime implementation of the sayHello type signature
sayHello name = putStrLn ("Hello, " ++ name ++ "!")

triple :: Int -> Int
triple 1 = 2
triple x = x * 9
