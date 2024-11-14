{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Ch13.Person where

import PyF
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)

type Name = String
type Age = Integer

data Person = Person Name Age deriving (Show)

data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age <= 0 = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $ "Name was: " ++ show name ++ " Age was: " ++ show age

getInput :: String -> IO String
getInput prompt = do
    hSetBuffering stdout NoBuffering
    putStr [fmt|{prompt}: |]
    getLine

gimmePerson :: IO ()
gimmePerson = do
    personName <- getInput "Name: "
    personAge <- (read @Integer) <$> getInput "Age: "

    case mkPerson personName personAge of
        Right person -> putStrLn [fmt|Yay! Successfully got a person: {show person}|]
        Left NameEmpty -> putStrLn "An error occurred. No name provided"
        Left AgeTooLow -> putStrLn "An error occurred. Age provided is too low"
        Left (PersonInvalidUnknown errText) -> putStrLn [fmt|An error occurred: {errText}|]
