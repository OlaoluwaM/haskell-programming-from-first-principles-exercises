{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Ch16.Playground where

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap f (Yeppers a) = Yeppers (f a)
    fmap _ LolNope = LolNope

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

newtype Mu f = InF {outF :: f (Mu f)}

data Sum b a = First a | Second b

instance Functor (Sum b) where
    fmap f (First a) = First (f a)
    fmap _ (Second b) = Second b

data Company a c b = DeepBlue a c | Something b

instance Functor (Company a c) where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More b) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant b a = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant b) where
    fmap f (Desk a) = Desk (f a)
    fmap _ (Bloor b) = Bloor b
    fmap _ Finance = Finance

newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K b) where
    fmap f (Flip (K a)) = Flip $ K (f a)

newtype EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst a) = GoatyConst (f a)

newtype LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
    fmap f (LiftItOut inner) = LiftItOut $ fmap f inner

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa inner inner') = DaWrappa (fmap f inner) (fmap f inner')

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething inner inner') = IgnoringSomething inner (fmap f inner')

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious inner1 inner2 inner3) = Notorious inner1 inner2 (fmap f inner3)

data List a = Nil | Cons a (List a)

instance Functor List where
    fmap f (Cons a restOfList) = Cons (f a) $ fmap f restOfList
    fmap _ Nil = Nil

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat goat) = OneGoat (f goat)
    fmap f (MoreGoats goat1 goat2 goat3) = MoreGoats (fmap f goat1) (fmap f goat2) (fmap f goat3)

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print str a) = Print str (f a)
    fmap f (Read readFn) = Read $ fmap f readFn
