{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Free where
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

data Term f a = Pure a
              | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Impure e) = Impure $ fmap (fmap f) e
instance Functor f => Applicative (Term f) where
  pure = Pure
  Pure f <*> x = fmap f x
  Impure f <*> x = Impure $ fmap (<*> x) f
instance Functor f => Monad (Term f) where
  return = pure
  Pure x >>= f = f x
  Impure e >>= f = Impure $ fmap (>>= f) e
