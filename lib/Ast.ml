module type FMap = sig
  type 'node t

  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module type ImplAlgs = sig
  type 'node t

  val eval : int t -> int
end

module type Node = sig
  include FMap
  include ImplAlgs with type 'node t := 'node t
end

module Mu (F : FMap) = struct
  type t = In of t F.t

  let rec cata alg (In e) = alg @@ F.fmap (cata alg) e
end

module Either (U : Node) (V : Node) = struct
  type 'node t = Left of 'node U.t | Right of 'node V.t

  let fmap f = function
    | Left x -> Left (U.fmap f x)
    | Right x -> Right (V.fmap f x)

  let eval = function Left x -> U.eval x | Right x -> V.eval x
end

module Val = struct
  type _ t = Val of int

  let fmap _ (Val x) = Val x
  let eval (Val x) = x
end

module Add = struct
  type 'node t = Add of 'node * 'node

  let fmap f (Add (a, b)) = Add (f a, f b)
  let eval (Add (a, b)) = a + b
end

module Expr = struct
  module E = Either (Val) (Add)
  include Mu (E)

  let eval_expr = cata E.eval
end
