open Dtalacarte.Ast

let expr : Expr.t = In (Right (Add (In (Left (Val 1)), In (Left (Val 2)))))

let rec eval : Expr.t -> int = function
  | In (Left (Val n)) -> n
  | In (Right (Add (a, b))) -> eval a + eval b

let _ =
  print_newline ();
  Printf.printf "%d" @@ eval expr;
  print_newline ();
