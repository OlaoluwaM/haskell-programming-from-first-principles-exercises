# Chapter 18

## Short Exercise: Either Monad

Implement the `Either` Monad:

```haskell
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure = Second
    (First a) <*> _ = First a
    _ <*> (First a) = First a
    (Second f) <*> (Second b) = Second (f b)

instance Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second b) >>= f = f b
```

## Section 18.7

Write Monad instances for the following types. Use the QuickCheck properties we showed you to validate your instances.

[Instance definitions can be found here](./InstancesEx.hs), and the code checking the lawfulness of those definition is [here](../../test/Ch18/InstancesExSpec.hs)
