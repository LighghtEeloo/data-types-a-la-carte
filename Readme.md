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

### Catamorphism (Fold)

Our first goal is to re-invent recursion. Borrowing the (bad) name from [Fantastic Morphisms and Where to Find Them: A Guide to Recursion Schemes](https://arxiv.org/abs/2202.13633), we can use `catamorphism` to create a fix-point on inductive data types `Mu` that folds an algebra over a term.

```haskell
newtype Mu f = In (f (Mu f))
cata :: Functor f => (f a -> a) -> Mu f -> a
cata f (In e) =
  f $ fmap (cata f) e
```

Now the only input we need is `f :: * -> *` that takes the `Mu f` itself and returns the node. We'll come back later.

### Either

To build an AST, we need to have a generalized `Sum` type - which is `Either`!

```haskell
data (a :+: b) e = Left (a e) | Right (b e)
```

Well, a form of `Either` that take an extra type parameter for the use of `Mu`. And written in a fancy form.

## Introduction and Elimination Forms

With the tools in hand, we can try and build the term constructors:

```haskell
type Expr = Mu (Val :+: Add)

newtype Val e = Val Int
val :: Int -> Expr
val n = In (Left (Val n))

data Add e = Add e e
add :: Expr -> Expr -> Expr
add x y = In (Right (Add x y))
```

Hmmm. Indeed we have made the nodes to be separated, but even if we only have two nodes, the `Left`s and `Right`s and the outermost `In` look annoying. If we add more nodes to the structure the complexity will soon pile up. We deserve better.

And term consumers:

```haskell
eval :: Expr -> Int
eval (In (Left (Val n))) = n
eval (In (Right (Add x y))) = eval x + eval y
```

Still far from satisfactory. So let's hide all the ugly details from the language implementor.

## `Eval` Algebra

The first thing we can notice is that for any "node" in our AST, the evaluation doesn't need to depend on the definition of the `Expr`. Because if there is a field that needs to, it's a recursive call to `eval` the `Expr` part, which has been avoided by `cata`.

As a result, we can give the definition of `Eval` by:

```haskell
class (Functor f) => Eval f where
  eval :: f Int -> Int
```

An intuition could be we will only run `eval` on `data Add Int = Add Int Int`, which is just `(+)`, and similar for all `f` passed into `Mu f`.

The reason for the constraint of `Functor f` comes from `cata`.

## Injection

To avoid the unsatisfactory constructors, we can use injection.

```haskell
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub e -> sup e

instance Functor f => f :<: f where
  inj = id
instance (Functor f, Functor g) => g :<: (f :+: g) where
  inj = Right
instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, f :<: g) => f :<: (g :+: h) where
  inj = Left . inj
```

## Monads for Free

