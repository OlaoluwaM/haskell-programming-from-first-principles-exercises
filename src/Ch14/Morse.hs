module Ch14.Morse where

import Control.Monad (forever, when)
import Data.Map (Map)
import Data.Map qualified as Map
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (isEOF)

newtype Morse = MorseCode {morseString :: String} deriving (Eq, Show, Ord)

letterToMorseMap :: Map Char Morse
letterToMorseMap =
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

morseToLetterMap :: Map Morse Char
morseToLetterMap = Map.foldrWithKey (flip Map.insert) Map.empty letterToMorseMap

charToMorse :: Char -> Maybe Morse
charToMorse = flip Map.lookup letterToMorseMap

morseToChar :: Morse -> Maybe Char
morseToChar = flip Map.lookup morseToLetterMap

stringToMorse :: String -> Maybe [Morse]
stringToMorse = mapM charToMorse

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
      (Just morseCodes) -> let morseStrings = morseString <$> morseCodes in putStrLn $ unwords morseStrings
      Nothing -> putStrLn ("Error: " <> line) >> exitFailure

convertFromMorse :: IO ()
convertFromMorse = do
  endOfData <- isEOF
  when endOfData exitSuccess

  line <- getLine
  convertLine line
 where
  convertLine line = do
    let decodedMorse = traverse (morseToChar . MorseCode) (words line)
    case decodedMorse of
      (Just str) -> putStrLn str
      Nothing -> putStrLn ("Error: " <> line) >> exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    ["from"] -> convertFromMorse
    ["to"] -> convertToMorse
    _ -> argError
 where
  argError =
    putStrLn
      "Please specify the\
      \ first argument\
      \ as being 'from' or\
      \ 'to' morse,\
      \ such as: morse to"
      >> exitFailure
