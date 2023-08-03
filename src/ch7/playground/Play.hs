{-# LANGUAGE LambdaCase #-}

module Play where

import Control.Monad hiding (Reader, State, Writer)

type RPNOperation = String

-- ["10","4","3","+","2","*","-"]
solveRPN :: RPNOperation -> Integer
solveRPN = head . foldl rpnParser [] . words
  where
    rpnParser (x : y : xs) "*" = x * y : xs
    rpnParser (x : y : xs) "-" = y - x : xs
    rpnParser (x : y : xs) "+" = x + y : xs
    rpnParser stack numberString = read numberString : stack

main = putStrLn "Your Input" >> getLine >>= print . solveRPN

-- heathrowToLondon :: String -> [Integer]
-- heathrowToLondon = map foo . chunksOf 3 . map parseInteger . words
--   where
--     foo [x] = (x,)
--     foo [x, y] = (x, y)
--     foo [x, y, z] = ()

-- parseInteger :: String -> Integer
-- parseInteger = read

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

concat' :: List a -> List a -> List a
concat' Nil Nil = Nil
concat' Nil consCell = consCell
concat' consCell Nil = consCell
concat' (Cons a l) consCell = Cons a $ concat' l consCell

f = concat' (Cons 2 Nil) (Cons 3 Nil)



-- tom :: Reader String String
-- tom = ask >>= \env -> return (env ++ " This is Tom")

-- jerry :: Reader String String
-- jerry = ask >>= \env -> return (env ++ " This is Jerry")

-- tomAndJerry :: Reader String String
-- tomAndJerry = do
--   tom' <- tom
--   jerry' <- jerry
--   return (tom' ++ "    " ++ jerry')
