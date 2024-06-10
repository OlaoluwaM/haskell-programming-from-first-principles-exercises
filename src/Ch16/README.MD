# Chapter 16 Exercises

## Exercises: Be Kind

Given a type signature, determine the kinds of each type variable:

1. (**Q**) What's the kind of `a`? | (**A**) `a` is of the kind `Type` in the type signature `f :: a -> a`
2. What are the kinds of `b` and `T`? (The `T` is capitalized on purpose!)

    In the type signature `f :: a -> b a -> T (b a)`, `b` inhabits the kind `Type -> Type`, while `T` is of kind `Type -> Type` as well

3. (**Q**) What's the kind of `c` | (**A**) `c` is of the kind `Type -> Type -> Type` in the type signature `f :: c a b -> c a b`
