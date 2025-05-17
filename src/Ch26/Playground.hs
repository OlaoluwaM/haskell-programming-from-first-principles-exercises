{-# LANGUAGE TupleSections #-}

module Ch26.Playground where

import Control.Monad (liftM, (>=>), guard)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (runIdentity))
import Control.Monad.Trans.Maybe qualified as MT

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
    fmap f (MaybeT mMa) = MaybeT $ (fmap . fmap) f mMa

instance (Applicative m) => Applicative (MaybeT m) where
    pure = MaybeT . pure . pure

    (MaybeT mMf) <*> (MaybeT mMa) = MaybeT $ liftA2 (<*>) mMf mMa

instance (Monad m) => Monad (MaybeT m) where
    (MaybeT mMa) >>= f = MaybeT $ mMa >>= maybe (pure Nothing) (runMaybeT . f)

newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT mEa) = EitherT $ (fmap . fmap) f mEa

instance (Applicative m) => Applicative (EitherT e m) where
    pure = EitherT . pure . pure

    (EitherT mEf) <*> (EitherT mEa) = EitherT $ liftA2 (<*>) mEf mEa

instance (Monad m) => Monad (EitherT e m) where
    (EitherT mEa) >>= f = EitherT $ mEa >>= either (pure . Left) (runEitherT . f)

swapEitherTM :: (Monad m) => EitherT e m a -> EitherT a m e
swapEitherTM (EitherT mE) = EitherT $ do
    e <- mE
    pure $ case e of
        Left a -> Right a
        Right e' -> Left e'

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT me) = EitherT $ fmap (either Right Left) me

eitherT :: (Monad m) => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g et = runEitherT et >>= either f g

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT fma) = ReaderT $ (fmap . fmap) f fma

instance (Applicative m) => Applicative (ReaderT e m) where
    pure = ReaderT . pure . pure

    (ReaderT fmf) <*> (ReaderT fma) = ReaderT $ liftA2 (<*>) fmf fma

instance (Monad m) => Monad (ReaderT r m) where
    (ReaderT fma) >>= f = ReaderT $ \r -> fma r >>= \a -> runReaderT (f a) r

newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT fmt) = StateT $ (fmap . fmap) (first f) fmt

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ pure . (a,)

    (StateT fmf) <*> (StateT fma) = StateT $ \initialState -> do
        (f, newState) <- fmf initialState
        first f <$> fma newState

instance (Monad m) => Monad (StateT s m) where
    (StateT fma) >>= f = StateT $ fma >=> (\(a, newS) -> runStateT (f a) newS)

-- Non-normalized version of the above
-- (StateT fma) >>= f = StateT $ \initialS -> fma initialS >>= (\ (a, newS) -> runStateT (f a) newS)

-- MaybeT (ExceptT String (ReaderT () IO)) Int
-- (ExceptT String (ReaderT () IO)) (Maybe Int)
-- (ReaderT () IO) (Either String (Maybe Int))
-- \() -> IO (Either String (Maybe Int))
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = (MaybeT . ExceptT . ReaderT) (const . pure . Right . Just $ 1)

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT (ExceptT (ReaderT (const (pure (Right (Just 1))))))

instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance MonadTrans (EitherT e) where
    lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
    lift ma = StateT $ \s -> liftM (,s) ma

instance MonadTrans (ReaderT r) where
    lift = ReaderT . const

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO

-- foo :: ReaderT r Maybe Int
-- foo = ReaderT $ \_ -> Just 1

-- baz :: MaybeT (Reader r) Int
-- baz = MaybeT $ pure $ Just 1

type Reader r a = ReaderT r Identity a

runReader ::
    -- | A @Reader@ to run.
    Reader r a ->
    -- | An initial environment.
    r ->
    a
runReader m = runIdentity . runReaderT m

rDec :: (Num a) => Reader a a
rDec = ReaderT $ pure . subtract 1

rShow :: (Show a) => Reader a String
rShow = ReaderT $ pure . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStrLn $ "Hi: " <> show a
    pure (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
    let x = show s
    putStrLn $ "Hi: " <> x
    pure (x, s + 1)

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MT.MaybeT IO String
maybeExcite = do
    v <- liftIO getLine
    guard $ isValid v
    pure v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- MT.runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e -> putStrLn ("Good, was very excite: " ++ e)
