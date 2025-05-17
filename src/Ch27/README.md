# Chapter 27

## Section 27.14

### What will `sprint` output

1. `let x = 1`

Will output `_` because numbers are polymorphic by default and thus the type of `x` is `Num a => a`, not some concrete type

2. `let x = ['1']`

Will output `"1"` due to the expression being applicable to opportunistic strictness. `"1"` is semantically equivalent to `['1']`

3. `let x = [1]`

Will output `_` cause the expression is polymorphic with the type `Num a => [a]`

4. `let x = 1 :: Int`

Will output `1`, opportunistic strictness

5. `let f = \x -> x; let x = f 1`

Will be `_` since `x` is bound to a function invocation

6. `let f :: Int -> Int; f = \x -> x; let x = f 1`

Will be `_` again because `x` is bound to a function call which does not benefit from opportunistic strictness

### WIll printing this expression result in bottom?

1. `snd (undefined, 1)`

Will not bottom out because the first tuple item will be ignored so we'll always get back `1`

2. ``let x = undefined; let y = x`seq` 1 in snd (x, y)``

Will bottom out, due to demand. `snd (x, y)` will demand `y` which in turn will demand `x` which id `undefined`

3. `length $ [1..5] ++ undefined`

Will bottom out due to attempting to incorporate `undefined` as part of the spine of the list and `length` being strict on the spine

4. `length $ [1..5] ++ [undefined]`

Will not bottom out (returns `6`) since we're now attempting to incorporate another list with our current list. The spines are intact so `length` should work as we'd expect

5. `const 1 undefined`

Will not bottom (returns `1`) since the second argument to any `const` is always discarded. It being `undefined` has not effect on those semantics

6. ``const 1 (undefined `seq` 1)``

Will not bottom out for similar reasons as described above.

7. `const undefined 1`

Will bottom out since the first argument to `const` is `undefined`

### Make the expression bottom

```haskell
x = undefined
y = x `seq` "blah"

main :: IO ()
main = print (snd (x, y))
```

Use a `seq` to artificially demand `x` from `y`
