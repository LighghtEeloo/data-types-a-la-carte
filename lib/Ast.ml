module type FMap = sig
  type 'node t
  val fmap : ('a -> 'b) -> 'a t -> 'b t
end

module Mu (F: FMap) = struct
  type t = In of (t F.t)
  let rec cata alg (In(e)) =
    alg @@ F.fmap (cata alg) e
end

module Either (U: FMap) (V: FMap) = struct
  type 'node t =
    | Left of 'node U.t
    | Right of 'node V.t
  let fmap f = function
    | Left(x) -> Left(U.fmap f x)
    | Right(x) -> Right(V.fmap f x)
end

module Val = struct
  type _ t = Val of int
  let fmap _ (Val(x)) = Val(x)
end

module Add = struct
  type 'node t = Add of 'node * 'node
  let fmap f (Add(a, b)) = Add(f a, f b)
end

module Expr = Mu (Either (Val) (Add))

