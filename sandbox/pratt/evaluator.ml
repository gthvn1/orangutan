let rec eval (expr : Parser.expr) : int =
  match expr with
  | Int i -> i
  | Add (e1, e2) -> eval e1 + eval e2
  | Sub (e1, e2) -> eval e1 - eval e2
  | Mul (e1, e2) -> eval e1 * eval e2
  | Div (e1, e2) ->
      let divisor = eval e2 in
      if divisor = 0 then failwith "Error: divide by zero";
      eval e1 / divisor
  | Neg e -> -eval e
