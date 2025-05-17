{-# LANGUAGE Strict #-}

module Ch27.StrictList where

data List a = Nil | Cons a (List a)
    deriving (Show)

take' :: Integer -> List a -> List a
take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

map' :: (t -> a) -> List t -> List a
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' :: p -> List p
repeat' x = xs where xs = Cons x xs

twoEls :: List Integer
twoEls = Cons 1 (Cons undefined Nil)

oneEl :: List Integer
oneEl = take' 1 twoEls

threeElements :: List Integer
threeElements = Cons 2 twoEls

oneElT :: List Integer
oneElT = take' 1 threeElements

main :: IO ()
main = print @(List Integer) $ take' 10 $ map' (+ 1) (repeat' 1)

--- >>> oneElT
-- Prelude.undefined
