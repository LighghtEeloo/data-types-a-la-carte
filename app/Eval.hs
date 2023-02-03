{-# LANGUAGE TypeOperators #-}

module Eval where
import Prelude hiding (Either, Left, Right)

newtype Mu f = In (f (Mu f))
foldExpr :: Functor f => (f a -> a) -> Mu f -> a
foldExpr f (In e) =
  f $ fmap (foldExpr f) e

data (a :+: b) e = Left (a e) | Right (b e)
instance (Functor a, Functor b) => Functor (a :+: b) where
  fmap f (Left x) = Left $ fmap f x
  fmap f (Right x) = Right $ fmap f x


type Expr = Mu (Val :+: Add)


class (Functor f) => Eval f where
  eval :: f Int -> Int
instance (Eval a, Eval b) =>  Eval (a :+: b) where
  eval (Left x) = eval x
  eval (Right x) = eval x


newtype Val e = Val Int
val :: Int -> Expr
val n = In (Left (Val n))

instance Functor Val where
  fmap _ (Val n) = Val n
instance Eval Val where
  eval (Val n) = n


data Add e = Add e e
add :: Expr -> Expr -> Expr
add x y = In (Right (Add x y))

instance Functor Add where
  fmap f (Add a b) = Add (f a) (f b)
instance Eval Add where
  eval (Add a b) = a + b

evalExpr :: Expr -> Int
evalExpr = foldExpr eval



ex :: Expr
ex = add (val 1) (val 2)

res :: Int
res = evalExpr ex
