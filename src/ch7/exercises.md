# Chapter 7 Exercises

## Variety Pack

```haskell
k (x, y) = x
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)
```

1a. `(a, b) -> a`
1b. The type of `k2` is `String`. No, it does not inhabit the same type as `k1` or `k3`
1c. `k1` and `k2`

2. Fill in the definition

```haskell
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
```

## Chapter Exercises

### Multiple Choice

1. d
2. b
3. d
4. b
5. a
