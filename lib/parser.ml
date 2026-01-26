let ping () = "pong"

module Ast = struct
  type identifier = Token.t * string
  type expression = unit (* We don't know yet *)
  type statement = Let of Token.t * identifier * expression
  type program = statement list
  type node = I of identifier | E of expression | S of statement

  (** [token_literal node] returns the literal value of the token associated to
      the [node]. It is used for debugging and testing. *)
  let token_literal node =
    match node with
    | I _ -> print_endline "TODO: implement token_literal for identifier"
    | S _ -> print_endline "TODO: implement token_literal for statement"
    | E _ -> print_endline "TODO: implement token_literal for expression"
end

(* Let statement:
  - A statement doesn't produce a result (<> from statement)
  - Let statement has three parts:
    - a "Let" keyword
    - an identifier
    - an expression
  *)
