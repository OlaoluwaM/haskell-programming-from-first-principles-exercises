{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Ch23.MyState where

import Data.Bifunctor (first)

newtype MyState s a = MyState {runMyState :: s -> (a, s)}

instance Functor (MyState s) where
    fmap f (MyState g) = MyState $ first f . g

instance Applicative (MyState s) where
    pure a = MyState $ \s -> (a, s)

    (MyState f) <*> (MyState g) = MyState $ \s ->
        let (mapFn, newS) = f s
            (output, finalS) = g newS
         in (mapFn output, finalS)

instance Monad (MyState s) where
    (MyState f) >>= g = MyState $ \s -> let (a, newS) = f s in runMyState (g a) newS
