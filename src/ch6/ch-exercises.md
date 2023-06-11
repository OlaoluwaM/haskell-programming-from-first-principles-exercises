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

1. Incompatible, because `i = 1` implies that the variable `i` *promises* to house a value of a type that satisfies the constraint of having an instance of the `Num` type class. By performing the assertions `i :: a`, we are then trying to say that the variable `i` *could* give us a value of any type, regardless of whether that type satisfies the `Num` constraint, which is not true

2. Incompatible for the similar reasons as the previous question. In this case, the variable `f`, without the `Float` assertion is supplied a constraint instead of a concrete type, a constraint of `Fractional a => a`. The reason why specializing `f` to the type `Float` works is because the type `Float` has an instance of the constraint `Fractional a => a`.
   However, attempting to assert `f` to the constraint `Num a => a` fails, because it is a logically unsound operation. `f`'s default type, the constraint `Fractional a => a` *promises* that the variable `f` **only** houses a value of a type that has an instance of the `Fractional` type class.
   Hence, by performing the assertion `f :: Num a => a`, we are attempting to override the original *promise* of `f` and instead say that the variable `f` *can* be any type that has an instance of the `Num` type class, which, again, just isn't true.

3. These are compatible because it is a restatement of the truth. By default, `f = 1.0` is a polymorphic constant with the constraint of `Fractional a => a`. It can be specialized to the concrete type `Float` if needed, but the fact still remains that it is polymorphic with the `Fractional a => a` constraint

4. These are also compatible. It's interesting actually. We know by default `f = 1.0` is a polymorphic constant with the constraint `Fractional a => a`. This constraint states that `f` can be used wherever a type that satisfies the `Fractional` constraint (like `Float`) is required. It just so happens that, all the types that satisfy the `Fractional` constraint also satisfy the `RealFrac` constraint, though, this is the same case with the `Num` constraint. However, unlike `Num`, the types that satisfy both the `Fractional` and `RealFrac` constraints are exactly the same. Thus, the *promises* when it comes to specialization are equivalent between these two constraints

5. These are compatible because the terms do not care about the constraint of the type argument. They are not used in any context specific to a particular constraint. At the term level, `freud` will behave the same regardless of the input type (Parametricity)

6. These are compatible as one is simply a specialization of the latter

7. These are compatible as one is simply a specialization of the latter

8. These are incompatible, because `x` could be specialized to a type that isn't `Int`, so it is unsound.

9. These are compatible as one is a simple specialization of the latter

10. Incompatible because, the first expression of the function has it's parameter type specialized to `[Char]` (AKA `String`), that is, we have promised that the function `young` only operates on values that inhabit the type `String`. The assertion to the constraint `Ord a => [a] -> a` fails because what *it promises* to users of `young` is contradictory to what the original signature of `young`, `String -> Char`, *promises*

11. Incompatible for the same reasons as the previous question. *Promises* to the user of the function `signifier` differ as the former states that it will work only for values of type `String`, while the latter states that it will work no matter that type of argument as long as it is in a list, and has an `Ord` instance. You can clearly see the dissonance which makes the possible substitution unsound.


