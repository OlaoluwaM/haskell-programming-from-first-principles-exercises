module Ch14.Morse where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Traversable (traverse)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, isEOF, stdin)

newtype Morse = MorseCode String deriving (Eq, Show, Ord)

letterToMorse :: Map Char Morse
letterToMorse =
  Map.fromList $
    (MorseCode <$>)
      <$> [ ('a', ".-")
          , ('b', "-...")
          , ('c', "-.-.")
          , ('d', "-..")
          , ('e', ".")
          , ('f', "..-.")
          , ('g', "--.")
          , ('h', "....")
          , ('i', "..")
          , ('j', ".---")
          , ('k', "-.-")
          , ('l', ".-..")
          , ('m', "--")
          , ('n', "-.")
          , ('o', "---")
          , ('p', ".--.")
          , ('q', "--.-")
          , ('r', ".-.")
          , ('s', "...")
          , ('t', "-")
          , ('u', "..-")
          , ('v', "...-")
          , ('w', ".--")
          , ('x', "-..-")
          , ('y', "-.--")
          , ('z', "--..")
          , ('1', ".----")
          , ('2', "..---")
          , ('3', "...--")
          , ('4', "....-")
          , ('5', ".....")
          , ('6', "-....")
          , ('7', "--...")
          , ('8', "---..")
          , ('9', "----.")
          , ('0', "-----")
          ]

morseToLetter :: Map Morse Char
morseToLetter = Map.foldrWithKey (flip Map.insert) Map.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse = flip Map.lookup letterToMorse

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse

morseToChar :: Morse -> Maybe Char
morseToChar = flip Map.lookup morseToLetter

convertToMorse :: IO ()
convertToMorse = forever $ do
  weAreDone <- isEOF
  when weAreDone exitSuccess

  line <- getLine
  convertLine line
 where
  convertLine line = do
    let morse = stringToMorse line
    case morse of
      (Just (MorseCode str)) -> putStrLn $ intercalate " " str
      Nothing -> putStrLn $ "Error: " ++ line >> exitFailure
