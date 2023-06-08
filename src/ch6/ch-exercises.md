# Chapter Exercises

## Multiple Choice

1. c
2. b
3. a
4. c
5. a

## Does it type check

1. No, because the data type `Person` does not implement an instance of the `Show` type class
2. No, because the data type `Mood` does not implement an instance of the `Eq` type class
3. About `settleDown`
   1. Only values that inhabit the type `Mood` are acceptable inputs to the function `settleDown`
   2. Attempting to evaluate `settleDown 9` will result in an error (will not type check), because values with the constraint `Num a => a` do not inhabit the type `Mood`
      which is the type the parameter of the function `settleDown` is specialized to
   3. `Blah > Woot` will not type check because `Mood` has no implementation for the `Ord` type class
4. Yes, this expression type checks

## Given a datatype declaration, what can we do?

1. No, the expression does not type check because `"chases"` and `True` do not inhabit the parameter types `Rocks` and `Yeah`
2. This type checks
3. This expression type checks because the data type `Papu` implements an instance of the `Eq` type class
4. No, the expression does not type check because the data type `Papu` does not implement an instance of the `Ord` type class

## Match the types


