{-# LANGUAGE TypeOperators #-}

module Fold where
import Prelude hiding (Either, Left, Right)

newtype Mu f = In (f (Mu f))

data (a :+: b) e = Left (a e) | Right (b e)

type Expr = Mu (Val :+: Add)

newtype Val e = Val Int
val :: Int -> Expr
val n = In (Left (Val n))

data Add e = Add e e
add :: Expr -> Expr -> Expr
add x y = In (Right (Add x y))




ex :: Expr
ex = add (val 1) (val 2)

eval :: Expr -> Int
eval (In (Left (Val n))) = n
eval (In (Right (Add x y))) = eval x + eval y
