module Ch11.Phone where

-- Maybe try redefining this using type literals (https://downloads.haskell.org/ghc/9.2.5/docs/html/users_guide/exts/type_literals.html#type-level-literals) one day

import Data.Char qualified as Char
import Data.Function
import Data.List qualified as BaseList

------------------------------------ Types -----------------------------------------------
type Digit = Char
type Presses = Int
type DigitSequence = [Char]
type CharInput = (Digit, Presses)
type CharInputSeq = [CharInput]

data Button = Button {getDigit :: Digit, getDigitSeq :: DigitSequence} deriving (Eq, Show)

newtype PhoneConfig = PhoneConfig [Button] deriving (Eq, Show)

----------------------------------------------------------------------------------------------

------------------------------------ Utils -----------------------------------------------
filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst _ [] = []
filterFirst predicate (x : xs) = if predicate x then [x] else filterFirst predicate xs

leftTrim :: String -> String
leftTrim = BaseList.dropWhile Char.isSpace

removeNonLetters :: String -> String
removeNonLetters = BaseList.filter Char.isAlpha

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen pred' s = case BaseList.dropWhile pred' s of
  "" -> []
  s' -> w : splitWhen pred' s''
   where
    (w, s'') = BaseList.break pred' s'

----------------------------------------------------------------------------------------------

------------------------------------ Helpers -----------------------------------------------
calculatePressesForChar :: Button -> Char -> Presses
calculatePressesForChar (Button _ digitSeq) char = (+ 1) $ BaseList.length $ BaseList.takeWhile (/= char) digitSeq

invalidCharConstant :: Char
invalidCharConstant = '?'

----------------------------------------------------------------------------------------------
-- Inspiration from https://github.com/mvaldesdeleon/haskell-book/tree/master
phone :: PhoneConfig
phone = PhoneConfig [zero, one, two, three, four, five, six, seven, eight, nine, star, pound]
 where
  zero = Button '0' "+_0"
  one = Button '1' " 1"
  two = Button '2' "abc2"
  three = Button '3' "def3"
  four = Button '4' "ghi4"
  five = Button '5' "jkl5"
  six = Button '6' "mno6"
  seven = Button '7' "pqrs7"
  eight = Button '8' "tuv8"
  nine = Button '9' "wxyz9"
  star = Button '*' "^*"
  pound = Button '#' ".,#"

_convertCharToInputSequence :: PhoneConfig -> Char -> CharInputSeq
_convertCharToInputSequence phoneConfig@(PhoneConfig buttons) char
  | Char.isUpper char = ('*', 1) : _convertCharToInputSequence phoneConfig (Char.toLower char)
  | otherwise = handleFilterResult $ filterFirst (\(Button _ digitSeq) -> char `BaseList.elem` digitSeq) buttons
 where
  handleFilterResult :: [Button] -> CharInputSeq
  handleFilterResult [] = [(char, 1)]
  handleFilterResult buttons' = let targetButton = BaseList.head buttons' in [(getDigit targetButton, calculatePressesForChar targetButton char)]

_convertInputSequenceToChar :: PhoneConfig -> CharInputSeq -> Char
_convertInputSequenceToChar _ [] = invalidCharConstant
_convertInputSequenceToChar _ [('*', 1)] = invalidCharConstant
_convertInputSequenceToChar phoneConfig (('*', 1) : rest) = Char.toUpper $ _convertInputSequenceToChar phoneConfig rest
_convertInputSequenceToChar (PhoneConfig buttons) charInpSeq@((digitPressed, presses) : _)
  | BaseList.length charInpSeq == 1 = handleFilterResult presses $ filterFirst (findTheRightButton digitPressed) buttons
  | otherwise = invalidCharConstant
 where
  findTheRightButton digit' (Button buttonDigit _) = buttonDigit == digit'

  handleFilterResult _ [] = digitPressed
  handleFilterResult presses' buttons' = let (Button _ targetBttnDigitSeq) = BaseList.head buttons' in targetBttnDigitSeq BaseList.!! getCharIndexFromDigitSeq targetBttnDigitSeq presses'

  getCharIndexFromDigitSeq targetBttnDigitSeq' presses' =
    let seqLength = BaseList.length targetBttnDigitSeq' in (presses' - 1) `mod` seqLength

reverseTaps :: Char -> CharInputSeq
reverseTaps = _convertCharToInputSequence phone

taps :: CharInputSeq -> Char
taps = _convertInputSequenceToChar phone

convo :: [String]
convo = ["Wanna play 20 questions", "Ya", "U 1st haha", "Lol OK. Have u ever tasted alcohol", "Lol ya", "Wow ur cool haha. Ur turn", "OK. Do u think I am pretty Lol", "Lol ya", "Just making sure rofl ur turn"]

encodedConvo :: [[CharInputSeq]]
encodedConvo = BaseList.map (BaseList.map reverseTaps) convo

decodedConvo :: [String]
decodedConvo = BaseList.map (BaseList.map taps) encodedConvo

isDecodedConvoEqualToOriginalConvo :: Bool
isDecodedConvoEqualToOriginalConvo = decodedConvo == convo

cellPhonesDead :: String -> CharInputSeq
cellPhonesDead = BaseList.concatMap reverseTaps

fingerTaps :: CharInputSeq -> Presses
fingerTaps = BaseList.foldr ((+) . snd) 0

_mostPopularLetter :: String -> [Char]
_mostPopularLetter = BaseList.maximumBy (on compare BaseList.length) . BaseList.group . removeNonLetters . BaseList.sort

mostPopularLetter :: String -> Char
mostPopularLetter = BaseList.head . BaseList.nub . _mostPopularLetter

mostPopularLetterCost :: String -> Int
mostPopularLetterCost = BaseList.sum . BaseList.map (fingerTaps . reverseTaps) . _mostPopularLetter

coolestLtr :: [String] -> Char
coolestLtr = mostPopularLetter . BaseList.unwords

coolestWord :: [String] -> String
coolestWord = fst . BaseList.maximumBy (on compare snd) . getWordFreq . BaseList.filter (/= "") . BaseList.map removeNonLetters . getWords
 where
  getWords = BaseList.concatMap (splitWhen Char.isSpace)
  getWordFreq words'' = BaseList.map (\word -> (word, BaseList.length $ BaseList.filter (== word) words'')) words''

coolestWordCost :: String -> Presses
coolestWordCost = fingerTaps . cellPhonesDead . coolestWord . (: [])
