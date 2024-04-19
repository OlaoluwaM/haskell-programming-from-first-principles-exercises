module Ch13.Playground where

import Ch11.Cipher qualified as Cipher
import Control.Monad (forever, when)
import Data.Char (isAlpha, toLower)
import System.Exit (exitSuccess)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import System.Random (randomRIO)

getInput :: IO String
getInput = do
  hSetBuffering stdout NoBuffering
  putStr "Enter input: "
  getLine

ceaserCipherPrompt :: IO ()
ceaserCipherPrompt = do
  inp <- getInput
  randomOffset <- randomRIO (1, 2000) :: IO Int
  let cipherText = Cipher.cipherMsg randomOffset inp
  putStrLn cipherText

vigenereCipherPrompt :: IO ()
vigenereCipherPrompt = do
  inp <- getInput
  let cipherText = Cipher.vigenereCipher "fervbour" inp
  putStrLn cipherText

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getInput
  let reverseLine1 = reverse line1

  when (line1 /= reverseLine1) $ do
    putStrLn "Nope!"
    exitSuccess

  putStrLn "It's a palindrome"

toLowerCaseStr :: String -> String
toLowerCaseStr = map toLower

palindromeSentence :: IO ()
palindromeSentence = forever $ do
  rawInp <- getInput
  let normalizedInp = filter isAlpha $ toLowerCaseStr rawInp
  let reverseNormalizedInp = reverse normalizedInp

  if normalizedInp == reverseNormalizedInp
    then
      putStrLn "It's a palindrome"
    else
      putStrLn "Nope!" >> exitSuccess
