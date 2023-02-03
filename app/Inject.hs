{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Inject where
import Prelude hiding (Either, Left, Right)

newtype Mu f = In (f (Mu f))
fold :: Functor f => (f a -> a) -> Mu f -> a
fold f (In e) =
  f $ fmap (fold f) e

data (a :+: b) e = Left (a e) | Right (b e)
instance (Functor a, Functor b) => Functor (a :+: b) where
  fmap f (Left x) = Left $ fmap f x
  fmap f (Right x) = Right $ fmap f x

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub e -> sup e

instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => g :<: (f :+: g) where
  inj = Right
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = Left . inj

inject :: (g :<: f) => g (Mu f) -> Mu f
inject = In . inj

class (Functor f) => Eval f where
  eval :: f Int -> Int
instance (Eval a, Eval b) =>  Eval (a :+: b) where
  eval (Left x) = eval x
  eval (Right x) = eval x


type Expr = Mu (Val :+: Add)

newtype Val e = Val Int
val :: Int -> Expr
val n = inject $ Val n

instance Functor Val where
  fmap _ (Val n) = Val n
instance Eval Val where
  eval (Val n) = n


data Add e = Add e e
add :: Expr -> Expr -> Expr
add x y = inject $ Add x y

instance Functor Add where
  fmap f (Add a b) = Add (f a) (f b)
instance Eval Add where
  eval (Add a b) = a + b

evalExpr :: Expr -> Int
evalExpr = fold eval




ex :: Expr
ex = add (val 1) (val 2)

res :: Int
res = fold eval ex
