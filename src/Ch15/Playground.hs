module Ch15.Playground where

data Optional a = Nada | Only a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Nada <> a@(Only _) = a
  a@(Only _) <> Nada = a
  (Only a) <> (Only b) = Only $ a <> b

instance (Monoid a) => Monoid (Optional a) where
  mempty = Nada

-- Madlibs

newtype Adjective = Adjective String deriving (Eq, Show)
newtype Adverb = Adverb String deriving (Eq, Show)
newtype Noun = Noun String deriving (Eq, Show)
newtype Exclamation = Exclamation String deriving (Eq, Show)

madlibbin' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbin' (Exclamation e) (Adverb adv) (Noun noun) (Adjective adj) = e <> " ! he said " <> adv <> " as he jumped into his car " <> noun <> " and drove off with his " <> adj <> " wife. "

madlibbinBetter' :: Exclamation -> Adverb -> Noun -> Adjective -> String
madlibbinBetter' (Exclamation e) (Adverb adv) (Noun noun) (Adjective adj) = mconcat [e, " ! he said ", adv, " as he jumped into his car ", noun, " and drove off with his ", adj, " wife. "]
