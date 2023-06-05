-- This is a type signature
sayHello :: String -> IO ()

-- This is the runtime implementation of the sayHello type signature
sayHello name = putStrLn ("Hello, " ++ name ++ "!")

triple :: Int -> Int
triple 1 = 2
triple x = x * 9

x :: Integer
x = 10
f c = c + y * x
e = f 3
y = 4
