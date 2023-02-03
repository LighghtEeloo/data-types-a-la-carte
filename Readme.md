# Data Types à la Carte

Presenter: Yuchen Jiang

## Motivating Problem

Suppose we want to represent an AST of a developing language that needs to add nodes incrementally, what approach should we take?

## Good Old Days

Why not ADTs?

```haskell
data Expr = Val Int
          | Add Expr Expr

val :: Int -> Expr
val = Val

add :: Expr -> Expr -> Expr
add = Add

eval :: Expr -> Int
eval (Val n) = n
eval (Add x y) = eval x + eval y

```

Easy, intuitive and straight forward, but...

- Hard to add new structures - have to go through and change all the files
  - High Cohesion
- Can I write a partial function on some of them?
  - Can't represent a part of them; have to split to several ADTs
  - `Prelude.undefined`
- What if there's a new demand that requires side effects?
  - Have to rewrite most of them into a monad

## A Quick Inspection

What do we need ADT for representing the AST?

1. Recursion (both type level and term level)
2. Composability

It turns out that we can solve them one by one.

## Catamorphism

Our first goal is to re-invent recursion. Borrowing the (bad) name from [Fantastic Morphisms and Where to Find Them: A Guide to Recursion Schemes](https://arxiv.org/abs/2202.13633), we can use `catamorphism` to create a fix-point on inductive data types `Mu` that folds an algebra over a term.

```haskell
newtype Mu f = In (f (Mu f))
cata :: Functor f => (f a -> a) -> Mu f -> a
cata f (In e) =
  f $ fmap (cata f) e
```

Now the only input we need is `f :: * -> *` that takes the `Mu f` itself and returns the node. We'll come back later.

## 
