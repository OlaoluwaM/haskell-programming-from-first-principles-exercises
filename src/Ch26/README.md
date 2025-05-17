# Chapter 26

## Misc Exercises

### Exercises `EitherT`

```haskell
newtype EitherT e m a = EitherT {runEitherT :: m (Either e a)}

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT mEa) = EitherT $ (fmap . fmap) f mEa


instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT mEf) <*> (EitherT mEa) = EitherT $ liftA2 (<*>) mEf  mEa

instance Monad m => Monad (EitherT e m) where
  (EitherT mEa) >>= f = EitherT $ mEa >>= either (pure . Left) (runEitherT . f)

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mE) = EitherT $ fmap (either Right Left) mE

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g eT = runEitherT eT >>= either f g
```

### Exercises: `StateT`

```haskell
newtype StateT s m a = StateT {runStateT :: s -> m (a, s)}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT fmt) = StateT $ (fmap . fmap) (first f) fmt


instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ pure . (a,)

  (StateT fmf) <*> (StateT fma) = StateT $ \initialState -> do
    (f, newState) <- fmf initialState
    first f <$> fma newState

instance Monad m => Monad (StateT s m) where
  (StateT fma) >>= f = StateT $ fma >=> (\ (a, newS) -> runStateT (f a) newS)
```

### Exercise: Wrap it up

```haskell
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = (MaybeT . ExceptT . ReaderT) (const . pure . Right . Just $ 1)

embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT (ExceptT (ReaderT (const (pure (Right (Just 1))))))
```

### Exercises: Lift more

```haskell
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance MonadTrans (StateT s) where
  lift ma = StateT $ \s -> liftM (,s) ma
```

## 26.14: Chapter Exercises

```haskell
rDec :: Num a => Reader a a
rDec = ReaderT $ pure . subtract 1

rShow :: Show a => Reader a String
rShow = ReaderT $ pure . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStrLn $ "Hi: " <> show a
    pure (a + 1)
```
