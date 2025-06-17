module Ch30.Playground where

import Control.Exception
import Data.Typeable

handler :: SomeException -> IO ()
handler (SomeException e) = do
    print $ typeOf e
    putStrLn $ "We errored! It was: " <> show e

handler2 :: SomeException -> IO ()
handler2 (SomeException e) = do
    putStrLn $ "Running main caused an error! It was" <> show e
    putStrLn "Writing to another file..."
    writeFile "bbb.txt" "hi"
    putStrLn "Wrote to 'bbb.txt' file instead"

willItFail :: Integer -> IO (Either ArithException ())
willItFail = try . print . div 5

canICatch :: (Exception e) => e -> IO (Either e ())
canICatch = try . throwIO

noWhammies :: IO (Either SomeException ())
noWhammies = try undefined

megaButtums :: IO (Either SomeException ())
megaButtums = try $ return undefined

main :: IO ()
main = do
    either print print =<< megaButtums

-- willItFail 0
-- writeFile "zzz.txt" "hi" `catch` handler2
