open Dtalacarte.Ast

let expr : Expr.t = In (Right (Add (In (Left (Val 1)), In (Left (Val 2)))))

let _ =
  print_newline ();
  Printf.printf "%d" @@ Expr.eval_expr expr;
  print_newline ();
