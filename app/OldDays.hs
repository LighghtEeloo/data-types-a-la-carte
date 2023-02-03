module OldDays where

data Expr = Val Int
          | Add Expr Expr

val :: Int -> Expr
val = Val

add :: Expr -> Expr -> Expr
add = Add

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y
