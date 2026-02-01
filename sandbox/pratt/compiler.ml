let rec compile (expr : Parser.expr) : Stack_vm.insn list =
  match expr with
  | Int n -> [ Push n ]
  | Add (e1, e2) -> compile e1 @ compile e2 @ [ Add ]
  | Sub (e1, e2) -> compile e1 @ compile e2 @ [ Sub ]
  | Mul (e1, e2) -> compile e1 @ compile e2 @ [ Mul ]
  | Div (e1, e2) -> compile e1 @ compile e2 @ [ Div ]
  | Neg e -> compile e @ [ Neg ]
