# Apply Yourself Exercise

1. The type `[a] -> [a] -> [a]`, post application, becomes `[Char] -> [Char]`. This is because we partially apply the operator onto a value that inhabits the `String`, giving the compiler enough information to infer a more concrete type for the second parameter and return value
2. The type `Num a => a -> a -> a` would become `Num a => a -> a`. This is because numeric values are polymorphic and thus do not provide any additional information to the compiler that would help cement the type of the variable `a`
3. The type `Int ->  [a] -> [a]` would become `Int -> [Char]`. This is because partial application has given us enough information about the variable `[a]` for the compiler to infer it as a `String`
4. The type `Ord a  => a  -> a -> Bool` becomes the type `Int -> Bool` because partial application has given us enough information about the variable `a` that the compiler is able to infer a concrete type for the variable
5. The type `Ord a  => a  -> a -> Bool` becomes the type `Char -> Bool` because partial application has given us enough information about the variable `a`  that the compiler is able to infer a concrete type for the variable `a`. The concrete type `Char`
