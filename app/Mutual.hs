{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Mutual where

import Prelude hiding (Either, Left, Right)

newtype Mu f = In (f (Mu f))
fold :: Functor f => (f a -> a) -> Mu f -> a
fold f (In e) =
  f $ fmap (fold f) e

foldMutu :: Functor f => (f (a, b) -> a, f (a, b) -> b) -> (Mu f -> a, Mu f -> b)
foldMutu (f, g) =
  (fst . fold alg, snd . fold alg)
  where alg x = (f x, g x)

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
