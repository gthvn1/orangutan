let rec to_bytecode (expr : Parser.expr) : Stack_vm.insn list =
  match expr with
  | Int n -> [ Push n ]
  | Add (e1, e2) -> to_bytecode e1 @ to_bytecode e2 @ [ Add ]
  | Sub (e1, e2) -> to_bytecode e1 @ to_bytecode e2 @ [ Sub ]
  | Mul (e1, e2) -> to_bytecode e1 @ to_bytecode e2 @ [ Mul ]
  | Div (e1, e2) -> to_bytecode e1 @ to_bytecode e2 @ [ Div ]
  | Neg e -> to_bytecode e @ [ Neg ]
