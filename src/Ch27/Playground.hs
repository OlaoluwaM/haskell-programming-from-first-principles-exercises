module Ch27.Playground where

gg :: Integer
gg =
    let x = undefined
        y = x `seq` 1
     in snd (x, y)

x :: a
x = undefined

y :: String
y = x `seq` "blah"

main :: IO ()
main = print (snd (x, y))

--- >>> snd (x, y)
-- Prelude.undefined
