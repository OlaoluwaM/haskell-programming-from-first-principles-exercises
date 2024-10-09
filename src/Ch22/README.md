# Chapter 22

## Short Exercise: Warming Up

```haskell
cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = rev . cap

fmapped :: [Char] -> [Char]
fmapped = rev <$> cap

tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> rev <*> cap

tupledM :: [Char] -> ([Char], [Char])
tupledM = do
  a <- cap
  b <- rev
  pure (a, b)

tupledM' :: [Char] -> ([Char], [Char])
tupledM' = cap >>= \a -> rev <&> (a,)
```

## Exercise: Ask

```haskell
newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id
```

## Exercise: Reading Comprehension

1. Write liftA2 yourself. Think about it in terms of abstracting out the difference between getDogR and getDogR' if that helps.

    ```haskell
    myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
    myLiftA2 f fa fb = f <$> fa <*> fb
    ```

2. Write the following function (`asks`). Again, it is simpler than it looks.

    ```haskell
    asks :: (r -> a) -> Reader r a
    asks = Reader
    ```

3. Implement the Applicative for Reader.

    ```haskell
    newtype Reader r a = Reader { runReader :: r -> a }

    instance Functor (Reader r) where
      fmap f (Reader ra) = Reader (f . ra)

    instance Applicative (Reader r) where
      pure a = Reader (const a)

      (Reader rf) <*> (Reader ra) = Reader $ \r -> let f = rf r in  f . ra $ r
    ```

## Exercise: Reader Monad

1. Implement the Reader Monad.

    ```haskell
    instance Monad (Reader r) where
      (Reader ra) >>= fra = Reader $ \r -> (runReader . fra . ra $ r) r
    ```

2. Rewrite the monadic getDogRM to use your Reader datatype.

    ```haskell
    newtype HumanName = HumanName String deriving (Eq, Show)
    newtype DogName = DogName String deriving (Eq, Show)
    newtype Address = Address String deriving (Eq, Show)

    data Person = Person { humanName :: HumanName , dogName :: DogName , address :: Address } deriving (Eq, Show)

    data Dog = Dog { dogsName :: DogName, dogsAddress :: Address } deriving (Eq, Show)

    getDogRM :: Reader Person Dog
    getDogRM = do
      name <- asks dogName
      addy <- asks address
      pure $ Dog name addy

    getDogRA :: Reader Person Dog
    getDogRA = liftA2 Dog (asks dogName) (asks address)
    ```

## Chapter Exercises

Found [here](./ChapterEx.hs)
