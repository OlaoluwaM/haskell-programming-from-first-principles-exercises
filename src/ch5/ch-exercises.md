# Haskell From First Principles

## Chapter 5: Types

### 5.8 Chapter Exercises

#### Multiple Choice

1. c
2. a
3. b
4. c

#### Determine the type

1.a. `Num a => a`
1.b. `Num a => (a, String)`
1.c. `(Integer, String)`
1.d. `False` (correct answer was `Bool`)
1.e. `Num a => a` (correct answer was `Int`)
1.f. `Bool`

2. `Num a => a`
3. `Num a => a -> a`
4. `Fractional a => a`
5. `String`

#### Does it compile

1. Does not compile

```haskell
bigNum = (^) 5 $ 10
wahoo = bigNum
```

2. Compiles
3. Does not compile

```haskell
a = (+30)
b = 5
c = a 10
d = a 100
```

4. Does not compile

```haskell
c = 10
b = 10000 + c
a = 12 + b
```

#### Type variables or specific type constructors

1.1. Fully Polymorphic Type Variable
1.2. Concrete Type Constructor
1.3. Concrete Type Constructor

2.1. Fully Polymorphic Type Variable
2.2. Constrained Polymorphic Type Variable
2.3. Concrete Type Constructor

3.1. Fully Polymorphic Type Variable
3.2. Fully Polymorphic Type Variable
3.3. Concrete Type Constructor

#### Write a type signature

1. `[a] -> a`
2. `(Ord a) -> a -> a -> Bool`
3. `(x, y) -> x`

#### Given a type, write the function

1. `id`
2. `c a b = a`
3. Yes, this function is the same as (#2) because it also returns it's first argument: `c'' b a = b`
4. `c' a b = b`
5. `tail`
6. `co bToC aToB a = bToC (aToB a)`
7. `f aToC a = a`
8. `a' aToB a = aToB a`

#### Fix it

##### Sing.hs

[Sing](playground/sing.hs)

##### Arith3Broken.hs

[Arith3Broken](playground/arith3broken.hs)

#### Type-Kwon-Do

[Type-Kwon-Do](playground/typeKwonDo.hs)
