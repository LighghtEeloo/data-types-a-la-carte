{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Example where
import Prelude hiding (Either, Left, Right)

-- Cata.hs

newtype Mu f = In (f (Mu f))
foldExpr :: (Functor f) => (f a -> a) -> Mu f -> a
foldExpr alg (In e) =
  alg $ fmap (foldExpr alg) e

-- data L a l = Nil
--               | Cons a l
-- type List a = Mu (L a)
-- f :: * -> * == List Int
-- List Mu ()
-- newtype Mu (L a) = In (L a (Mu (L a)))

-- Either.hs
data (a :+: b) e = Left (a e) | Right (b e) deriving (Functor)

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub e -> sup e

instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => g :<: (f :+: g) where
  inj = Right
-- instance (Functor f, Functor g) => f :<: (f :+: g) where
--   inj = Left
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = Left . inj

inject :: (g :<: f) => g (Mu f) -> Mu f
inject = In . inj

-- Eval.hs

class (Functor f) => Eval f where
  eval :: f Int -> Int

instance (Eval a, Eval b) => Eval (a :+: b) where
  eval (Left x) = eval x
  eval (Right x) = eval x






-- Syntax.hs

newtype Val e = Val Int deriving (Functor)
val :: Int -> Expr
val x = inject $ Val x
instance Eval Val where
  eval (Val n) = n

data Add e = Add e e deriving (Functor)
add :: (Add :<: f) => Mu f -> Mu f -> Mu f
add x y= inject $ Add x y
instance Eval Add where
  eval (Add a b) = a + b

data Mul e = Mul e e deriving (Functor)
mul :: (Mul :<: f) => Mu f -> Mu f -> Mu f
mul x y= inject $ Mul x y
instance Eval Mul where
  eval (Mul a b) = a * b



type Expr = Mu (Val :+: Add :+: Mul)

evalExpr :: Expr -> Int
evalExpr = foldExpr eval


-- term :: Expr
-- term = In $ Left $ Right $ Add (In (Left (Left (Val 1)))) (In (Left (Left (Val 1))))
