# [Data Types à la Carte](https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409)

Author: Wouter Swierstra

Presenter: Yuchen Jiang

## Setup

This presentation uses `cabal`. To run, simply

```bash
cabal run
```

Most titles contain a link to the corresponding Haskell code. Maybe I should try literate programming sometime...

## Motivating Problem

Suppose we want to represent an AST of a developing language that needs to add nodes incrementally, what approach should we take?

## [Good Old Days](./app/OldDays.hs)

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
  - Have to rewrite most of them into various monads

## A Quick Inspection

What do we need ADT for representing the AST?

1. Recursion (both type level and term level)
2. Composability

It turns out that we can solve them one by one.

### [Catamorphism (Fold)](./app/Cata.hs)

Our first goal is to re-invent recursion. Borrowing the (bad) name from [Fantastic Morphisms and Where to Find Them: A Guide to Recursion Schemes](https://arxiv.org/abs/2202.13633), we can use `catamorphism` to create a fix-point on inductive data types `Mu` that folds an algebra over a term.

```haskell
newtype Mu f = In (f (Mu f))
cata :: Functor f => (f a -> a) -> Mu f -> a
cata f (In e) =
  f $ fmap (cata f) e
```

Now the only input we need is `f :: * -> *` that takes the `Mu f` itself and returns the node. We'll come back later.

### [Either](./app/Cata.hs)

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

## [`Eval` Algebra](./app/Eval.hs)

The first thing we can notice is that for any "node" in our AST, the evaluation doesn't need to depend on the definition of the `Expr`. Because if there is a field that needs to, it's a recursive call to `eval` the `Expr` part, which has been avoided by `cata`.

As a result, we can give the definition of `Eval` by:

```haskell
class (Functor f) => Eval f where
  eval :: f Int -> Int
```

The reason for the constraint of `Functor f` comes from `cata`. An intuition could be we will only run `eval` on `data Add Int = Add Int Int`, which is just `(+)`, and similar for all `f` passed into `Mu f`. `cata` will do the rest of the work for us.

After the definition of `Eval` we can implement it for all nodes that can be evaluated:

```haskell
instance Eval Val where
  eval (Val n) = n

instance Eval Add where
  eval (Add a b) = a + b
```

Easy.

## [Injection](./app/Inject.hs)

To avoid unsatisfactory constructors, we can use injection, which is a way to express the fact that "if we have the ability to build a smaller term in our generalized sum type, we can build the sum type". The injections is expressed in three instances:

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

The first instance shows that the injection is reflexive. The last two rules are responsible for the inductive definition of `:<:` over `:+:`. The only structure that the injection needs to penetrate is `Mu`:

```haskell
inject :: (f :<: g) => f (Mu g) -> Mu g
inject = In . inj
```

and we can just prepend `inject $` to any node and get its constructor automatically:

```haskell
val :: Int -> Expr
val n = inject $ Val n

add :: Expr -> Expr -> Expr
add x y = inject $ Add x y
```

No more lispy `In (Left (Right (Add (In (Left (Left (Val 1)))) (In (Left (Left (Val 1)))))))` anymore.

## What have we achieved?

With all the preparations, we can separate all algebra definitions and all data definitions into different files, and only implement the needed algebras for each data type. We can flexibly combine data type with `:+:` at any time and use the algebra with all required instances defined. The powerful flexivity is where this approach gets its name from - it's like ordering pre-deined dishes and combine them as you wish.

The rest of the paper will focus on the famous free monad, the holy grail of monadic operations.

## [Monads for Free](./app/Free.hs)

As long as a type constructor `f` has type class `Functor` defined, we can get `Monad` for `Term f`.

```haskell
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
```

This is called the **free monad** for two reasons.

1. We can get the monad for free.
2. The property of this monad is relative to the forgetful functor.

## [Encapsulating Side Effects](./app/FreeRun.hs)

We can then use `Term` to characterize side effects via state monads:

```haskell
type State a = Int -> (a, Int)
class (Functor f) => Run f where
  run :: f (State a) -> State a
instance (Run f, Run g) => Run (f :+: g) where
  run (Left x) = run x
  run (Right x) = run x

runTerm :: (Run f) => Term f a -> State a
runTerm = foldTerm ((,), run)
```

and later implement some operators:

```haskell
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
```

and finally write a program in the DSL we've just defined:

```haskell
-- tick :: Term (Recall :+: Incr :+: Clear) Int
tick :: (Recall :<: f, Incr :<: f) => Term f Int
tick = do
  x <- recall
  incr 1
  pure x
```

The comment is a concrete typed program, while the actual type annotation is less limited. Though the consequence follows:

```haskell
main1 :: IO ()
main1 = do
  print $ runTerm (tick :: Term (Recall :+: Incr) Int) 4
```

Our main program will need type annotation to determine which exact implementation to choose.

## Final Comments

The discovery of free monads eased a major source of painfulness when writing code in pure functional languages like Haskell - defining different monads all the time. This innovative approach of creating composable monads has established fame for the classical functional pearl paper.

## Lingering Questions

The paper introduced a better way to organize ADTs. What about GADTs?

Does the approach that the author suggested result in performance impact?

## Special Thanks

A fantastic [blog](https://iota.huohuo.moe/FreeMonad.html) (in Chinese though) by Niltok (玩火) that goes through the main ideas of the paper.
