# Chapter 16 Exercises

## Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. (**Q**) What's the kind of `a`? | (**A**) `a` is of the kind `Type` in the type signature `f :: a -> a`
2. What are the kinds of `b` and `T`? (The `T` is capitalized on purpose!)

    In the type signature `f :: a -> b a -> T (b a)`, `b` inhabits the kind `Type -> Type`, while `T` is of kind `Type -> Type` as well

3. (**Q**) What's the kind of `c` | (**A**) `c` is of the kind `Type -> Type -> Type` in the type signature `f :: c a b -> c a b`

## Exercises: Heavy Lifting

Add `fmap`, parentheses, and function composition to each expression as needed for the expression to type check and produce the expected result. It may not always need to go in the same place, so don’t become complacent:

1. `a = (+1) $ read "[1]" :: [Int]`

    ```haskell
    a :: [Int]
    a = fmap (+ 1) (read "[1]" :: [Int])
    ```

2. `b = (++ "lol") (Just ["Hi,", "Hello"])`

    ```haskell
    b :: Maybe [String]
    b = (fmap . fmap) (++ "lol") (Just ["Hi, ", "Hello"])
    ```

3. `c = (*2) (\x -> x - 2)`

    ```haskell
    c :: Integer -> Integer
    c = fmap (* 2) (\x -> x - 2)
    ```

4. `d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])`

    ```haskell
    d :: Integer -> [Char]
    d = fmap ((return '1' ++) . show) (\x -> [x, 1 .. 3])
    ```

5. `e :: IO Integer`

    ```haskell
    e :: IO Integer
    e = let ioi = readIO "1" :: IO Integer
            changed = read ("123"++) show ioi
        in (*3) changed

    -- Answer
    e :: IO Integer
    e = let ioi = readIO "1" :: IO Integer in fmap ((* 3) . read . ("123" ++) . show) ioi
    ```

## Section 16.17

### Determine whether a valid Functor can be written for the datatype provided

1. `data Bool = False | True` cannot have a valid functor instance because it is of kind `Type`
2. data `BoolAndSomethingElse a = False' a | True' a` can have a valid functor instance because it is of at least the kind `Type -> Type`
3. `data BoolAndMaybeSomethingELse a = Falsish | Truish a` can have a valid functor instance because it is of at least the kind `Type -> Type`. Is similar to `Maybe` which already has a functor instance
4. `newtype Mu f = InF { outF :: f (Mu f)}` cannot have a valid functor instance. Not even sure if the newtype definition is even valid since `f` is of the kind `Type -> Type`, but being used as though it were of kind `Type`
5. `data D = D (GHC.Arr.Array Word Word) Int Int` cannot have a valid functor instance because it is of kind `Type`

### Rearrange the arguments to the type constructor of the datatype so the Functor instance works

1. `data Sum a b = First a | Second b`

    ```haskell
    data Sum b a = First a | Second b

    instance Functor (Sum b) where
        fmap f (First a) = First (f a)
        fmap _ (Second b) = Second b
    ```

2. `data Company a b c = DeepBlue a c | Something b`

    ```haskell
    data Company a c b = DeepBlue a c | Something b

    instance Functor (Company a c) where
        fmap f (Something b) = Something (f b)
        fmap _ (DeepBlue a c) = DeepBlue a c
    ```

3. `data More a b = L a b a | R b a b deriving (Eq, Show)`

    ```haskell
    data More b a = L a b a | R b a b deriving (Eq, Show)

    instance Functor (More b) where
        fmap f (L a b a') = L (f a) b (f a')
        fmap f (R b a b') = R b (f a) b'
    ```

### Write Functor instances for the following data types

1. `data Quant a b = Finance | Desk a | Bloor b`

    ```haskell
    data Quant b a = Finance | Desk a | Bloor b deriving (Eq, Show)

    instance Functor (Quant b) where
        fmap f (Desk a) = Desk (f a)
        fmap _(Bloor b) = Bloor b
        fmap_ Finance = Finance
    ```

2. `newtype K a b = K a deriving (Eq, Show)`

    ```haskell
    newtype K a b = K a deriving (Eq, Show)

    instance Functor (K a) where
        fmap _ (K a) = K a
    ```

3. `K` with `newtype Flip f a b = Flip (f b a) deriving (Eq, Show)`

    ```haskell
    newtype K a b = K a deriving (Eq, Show)

    newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

    instance Functor (Flip K b) where
        fmap f (Flip (K a)) = Flip $ K (f a)
    ```

4. `newtype EvilGoateeConst a b = GoatyConst b`

    ```haskell
    newtype EvilGoateeConst a b = GoatyConst b

    instance Functor (EvilGoateeConst a) where
        fmap f (GoatyConst a) = GoatyConst (f a)
    ```

5. `newtype LiftItOut f a = LiftItOut (f a)`

    ```haskell
    newtype LiftItOut f a = LiftItOut (f a)

    instance (Functor f) => Functor (LiftItOut f) where
        fmap f (LiftItOut inner) = LiftItOut $ fmap f inner
    ```

6. `data Parappa f g a = DaWrappa (f a) (g a)`

    ```haskell
    data Parappa f g a = DaWrappa (f a) (g a)

    instance (Functor f, Functor g) => Functor (Parappa f g) where
        fmap f (DaWrappa inner inner') = DaWrappa (fmap f inner) (fmap f inner')
    ```

7. `data IgnoreOne f g a b = IgnoringSomething (f a) (g b)`

    ```haskell
    data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

    instance (Functor g) => Functor (IgnoreOne f g a) where
        fmap f (IgnoringSomething inner inner') = IgnoringSomething inner (fmap f inner')
    ```

8. `data Notorious g o a t = Notorious (g o) (g a) (g t)`

    ```haskell
    data Notorious g o a t = Notorious (g o) (g a) (g t)

    instance (Functor g) => Functor (Notorious g o a) where
        fmap f (Notorious inner1 inner2 inner3) = Notorious inner1 inner2 (fmap f inner3)
    ```

9. `data List a = Nil | Cons a (List a)`

    ```haskell
    data List a = Nil | Cons a (List a)

    instance Functor List where
        fmap f (Cons a restOfList) = Cons (f a) $ fmap f restOfList
        fmap _ Nil = Nil
    ```

10. `data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)`

    ```haskell
    data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

    instance Functor GoatLord where
        fmap _ NoGoat = NoGoat
        fmap f (OneGoat goat) = OneGoat (f goat)
        fmap f (MoreGoats goat1 goat2 goat3) = MoreGoats (fmap f goat1) (fmap f goat2) (fmap f goat3)
    ```

11. `data TalkToMe a = Halt | Print String a | Read (String -> a)`

    ```haskell
    data TalkToMe a = Halt | Print String a | Read (String -> a)

    instance Functor TalkToMe where
        fmap _ Halt = Halt
        fmap f (Print str a) = Print str (f a)
        fmap f (Read readFn) = Read $ fmap f readFn
    ```
