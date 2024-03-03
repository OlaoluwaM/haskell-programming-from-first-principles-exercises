module Ch11.HuttonsRazor where

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add expr1 expr2) = eval expr1 + eval expr2

printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add expr1 expr2) = printExpr expr1 ++ " + " ++ printExpr expr2
