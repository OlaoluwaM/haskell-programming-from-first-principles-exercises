# Chapter 13 Exercises

## Modifying code

1. Ciphers: Open your ciphers module, and modify it so that the Caesar and Vigenère ciphers work with user input.

    ```haskell
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
    ```

2. Here is a very simple, short block of code. Notice it has a forever that will make it keep running, over and over again. Load it into your REPL, and test it out. Then, refer back to the chapter, and modify it to exit successfully after a False result

    ```haskell
    import Control.Monad (forever, when)

    getInput :: IO String
    getInput = do
      hSetBuffering stdout NoBuffering
      putStr "Enter input: "
      getLine

    palindrome :: IO ()
    palindrome = forever $ do
      line1 <- getInput
      let reverseLine1 = reverse line1

      when (line1 /= reverseLine1) $ do
        putStrLn "Nope!"
        exitSuccess

      putStrLn "It's a palindrome"
    ```

3. If you try using palindrome on a sentence such as “Madam I’m Adam,” you may notice that it doesn’t work. Modifying the above so that it works on sentences, too, involves several steps. You may need to refer back to previous examples in the chapter to get ideas for proper ordering and nesting. You may wish to import Data.Char to use the function toLower. Have fun.

    ```haskell
    import Control.Monad (forever)
    import Data.Char (isAlpha, toLower)
    import System.Exit (exitSuccess)

    getInput :: IO String
    getInput = do
      hSetBuffering stdout NoBuffering
      putStr "Enter input: "
      getLine

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
    ```

4.[Person](Person.hs)
