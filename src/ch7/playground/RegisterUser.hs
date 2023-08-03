module RegisteredUser where

newtype Username = Username String

newtype AccountNumber = AccountNumber String

data User
  = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username u) (AccountNumber an)) = putStrLn $ "RegisteredUser " ++ u ++ " " ++ an

rUser = RegisteredUser myUser myAcctNum
  where
    myUser = Username "Bob"
    myAcctNum = AccountNumber "12345"

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

newtype Penguin
  = Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereTheyLive) = whereTheyLive

humbledolt = Peng SouthAmerica

gentoo = Peng Antarctica

macaroni = Peng Antarctica

little = Peng Australia

galapagos = Peng Galapagos

antarcticaOrGalapagos :: Penguin -> Bool
antarcticaOrGalapagos peng = case peng of
  (Peng Antarctica) -> result
  (Peng Galapagos) -> result
  _ -> False
  where
    result = True
