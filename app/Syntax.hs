{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Syntax where

newtype Expr f = In (f (Expr f))
data (f :+: g) e = InL (f e) | InR (g e) deriving (Functor)
cata :: Functor f => (f a -> a) -> Expr f -> a
cata f (In x) =
  f $ fmap (cata f) x

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub e -> sup e
instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => g :<: (f :+: g) where
  inj = InR
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = InL . inj

inject :: (g :<: f) => g (Expr f) -> Expr f
inject = In . inj

class Functor f => Eval f where
  evalAlg :: f Int -> Int
instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlg (InL a) = evalAlg a
  evalAlg (InR b) = evalAlg b

-- User defined data types as ast nodes

newtype Val e = Val Int deriving (Functor)
data Add e = Add e e deriving (Functor)
data Mul e = Mul e e deriving (Functor)

val :: (Val :<: f) => Int -> Expr f
val = inject . Val
add :: (Add :<: f) => Expr f -> Expr f -> Expr f
add x y = inject (Add x y)
mul :: (Mul :<: f) => Expr f -> Expr f -> Expr f
mul x y = inject (Mul x y)

instance Eval Val where
  evalAlg (Val v) = v

instance Eval Add where
  evalAlg (Add a b) = a + b

instance Eval Mul where
  evalAlg (Mul a b) = a * b

-- type E = Expr (Val :+: Add)
type E = Expr (Val :+: Add :+: Mul)

evalExpr :: E -> Int
evalExpr = cata evalAlg

-- v' :: E
-- v' = In . InL . InL . Val $ 1

re :: E
re = val 1

res :: Int
res = evalExpr $ add (val 1) (val 2)
