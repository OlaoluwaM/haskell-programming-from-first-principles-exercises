module Ch11.PhoneAlt where

import Data.Char

------------------------------------- Types ----------------------------------------------
type Digit = Char
type CharDigit = String
type Presses = Int
type CharInputSeq = [(Digit, Presses)]

data DaPhone = DaPhone {getInputSeqForChar :: Char -> CharInputSeq, getCharFromInputSeq :: CharInputSeq -> Char}

--------------------------------------------------------------------------------------------

------------------------------------ Utils -----------------------------------------------
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst pred' (x : xs) = if pred' x then [x] else filterFirst pred' xs

----------------------------------------------------------------------------------------------

----------------------------------- Helpers --------------------------------------------------
invalidCharConstant :: Char
invalidCharConstant = '?'

getPressesThresholdForDigit :: Digit -> Int
getPressesThresholdForDigit '1' = 2
getPressesThresholdForDigit '7' = 5
getPressesThresholdForDigit '9' = 5
getPressesThresholdForDigit _ = 4

normalizeInpSeq :: CharInputSeq -> CharInputSeq
normalizeInpSeq = map (\(digit, presses) -> (digit, presses `mod` getPressesThresholdForDigit digit))

-----------------------------------------------------------------------------------------------

defaultPhoneConfig :: DaPhone
defaultPhoneConfig = DaPhone{getInputSeqForChar = _defaultGetInputSeqForChar, getCharFromInputSeq = _defaultGetCharFromInputSeq}

_defaultGetInputSeqForChar :: Char -> CharInputSeq
_defaultGetInputSeqForChar ' ' = [('1', 1)]
_defaultGetInputSeqForChar '1' = [('1', 2)]
_defaultGetInputSeqForChar 'a' = [('2', 1)]
_defaultGetInputSeqForChar 'b' = [('2', 2)]
_defaultGetInputSeqForChar 'c' = [('2', 3)]
_defaultGetInputSeqForChar '2' = [('2', 4)]
_defaultGetInputSeqForChar 'd' = [('3', 1)]
_defaultGetInputSeqForChar 'e' = [('3', 2)]
_defaultGetInputSeqForChar 'f' = [('3', 3)]
_defaultGetInputSeqForChar '3' = [('3', 4)]
_defaultGetInputSeqForChar 'g' = [('4', 1)]
_defaultGetInputSeqForChar 'h' = [('4', 2)]
_defaultGetInputSeqForChar 'i' = [('4', 3)]
_defaultGetInputSeqForChar '4' = [('4', 4)]
_defaultGetInputSeqForChar 'j' = [('5', 1)]
_defaultGetInputSeqForChar 'k' = [('5', 2)]
_defaultGetInputSeqForChar 'l' = [('5', 3)]
_defaultGetInputSeqForChar '5' = [('5', 4)]
_defaultGetInputSeqForChar 'm' = [('6', 1)]
_defaultGetInputSeqForChar 'n' = [('6', 2)]
_defaultGetInputSeqForChar 'o' = [('6', 3)]
_defaultGetInputSeqForChar '6' = [('6', 4)]
_defaultGetInputSeqForChar 'p' = [('7', 1)]
_defaultGetInputSeqForChar 'q' = [('7', 2)]
_defaultGetInputSeqForChar 'r' = [('7', 3)]
_defaultGetInputSeqForChar 's' = [('7', 4)]
_defaultGetInputSeqForChar '7' = [('7', 5)]
_defaultGetInputSeqForChar 't' = [('8', 1)]
_defaultGetInputSeqForChar 'u' = [('8', 2)]
_defaultGetInputSeqForChar 'v' = [('8', 3)]
_defaultGetInputSeqForChar '8' = [('8', 4)]
_defaultGetInputSeqForChar 'w' = [('9', 1)]
_defaultGetInputSeqForChar 'x' = [('9', 2)]
_defaultGetInputSeqForChar 'y' = [('9', 3)]
_defaultGetInputSeqForChar 'z' = [('9', 4)]
_defaultGetInputSeqForChar '9' = [('9', 5)]
_defaultGetInputSeqForChar char
  | isUpper char = ('*', 1) : _defaultGetInputSeqForChar (toLower char)
  | otherwise = [(char, 1)]

_defaultGetCharFromInputSeq :: CharInputSeq -> Char
_defaultGetCharFromInputSeq [] = invalidCharConstant
_defaultGetCharFromInputSeq inpSeq = getChar' $ filterFirst ((== normalizedInpSeq) . snd) allCharInpSeqWithChar
 where
  allChars = ' ' : (['a' .. 'z'] ++ ['A' .. 'Z'])
  allCharInpSeqWithChar = map (\char -> (char, _defaultGetInputSeqForChar char)) allChars
  normalizedInpSeq = normalizeInpSeq inpSeq

  getChar' :: [(Char, CharInputSeq)] -> Char
  getChar' [] = fst $ head inpSeq
  getChar' (inpSeq1 : _) = fst inpSeq1

convo :: [String]
convo = ["Wanna play 20 questions", "Ya", "U 1st haha", "Lol OK. Have u ever tasted alcohol", "Lol ya", "Wow ur cool haha. Ur turn", "OK. Do u think I am pretty Lol", "Lol ya", "Just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone getInpFromChar _) = getInpFromChar
