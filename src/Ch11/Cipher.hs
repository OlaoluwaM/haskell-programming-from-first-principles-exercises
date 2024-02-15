module Ch11.Cipher where

import Ch9.Cipher qualified as CeaserCipher

main :: IO ()
main = print $ CeaserCipher.cipherMsg 5 "A Pro Benji"
