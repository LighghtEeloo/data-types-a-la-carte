{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module FreeRun where
import Prelude hiding (Either, Left, Right)

data (a :+: b) e = Left (a e) | Right (b e) deriving (Functor)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub e -> sup e
instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => g :<: (f :+: g) where
  inj = Right
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = Left . inj


data Term f a = Pure a
              | Impure (f (Term f a))

foldTerm :: Functor f => (a -> b, f b -> b) -> Term f a -> b
foldTerm (f, _) (Pure a) = f a
foldTerm (f, g) (Impure e) = g $ fmap (foldTerm (f, g)) e

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

inject :: (f :<: g) => f (Term g a) -> Term g a
inject = Impure . inj


type State a = Int -> (a, Int)
class (Functor f) => Run f where
  run :: f (State a) -> State a
instance (Run f, Run g) => Run (f :+: g) where
  run (Left x) = run x
  run (Right x) = run x

runTerm :: (Run f) => Term f a -> State a
runTerm = foldTerm ((,), run)


newtype Recall t = Recall (Int -> t) deriving (Functor)
recall :: (Recall :<: f) => Term f Int
recall = inject $ Recall pure
instance Run Recall where
  run (Recall f) i = f i i

data Incr t = Incr Int t deriving (Functor)
incr :: (Incr :<: f) => Int -> Term f ()
incr n = inject $ Incr n (pure ())
instance Run Incr where
  run (Incr n f) i = f (i + n)

newtype Clear t = Clear t deriving (Functor)
clear :: (Clear :<: f) => Term f ()
clear = inject $ Clear (pure ())
instance Run Clear where
  run (Clear f) _ = f 0



-- tick :: Term (Recall :+: Incr :+: Clear) Int
tick :: (Recall :<: f, Incr :<: f) => Term f Int
tick = do
  x <- recall
  incr 1
  pure x



main1 :: IO ()
main1 = do
  print $ runTerm (tick :: Term (Recall :+: Incr) Int) 4
