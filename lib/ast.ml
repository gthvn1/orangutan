module Identifier = struct
  type t = { token : Token.t; value : string }
  (** [identifier] represents a variable name in Monkey. It consists of the
      token associated with the identifier (used for debugging/testing) and the
      string value of the identifier itself. *)

  let to_string ident = ident.value
end

module Expression = struct
  type t = unit
  (** [expression] represents expressions in Monkey. For now, the only
      expression is an identifier. Later, this will be extended to include
      literals, function calls, infix expressions, etc. *)

  let to_string _expr : string = "<TODO: expr to string>"
end

module Statement = struct
  type let_stmt = { token : Token.t; name : Identifier.t; value : Expression.t }
  type return_stmt = { token : Token.t; value : Expression.t }
  type expr_stmt = { token : Token.t; expression : Expression.t }

  (** [t] represents all statements in Monkey. Currently, the only statement
      type is 'let_statement', but more can be added later. *)
  type t = Let of let_stmt | Return of return_stmt | Expression of expr_stmt

  (** [token_literal node] returns the literal value of the token associated
      with the node. This is primarily used for debugging and testing, not for
      evaluation. *)
  let token_literal (stmt : t) : string =
    match stmt with
    | Let ls -> ls.token.literal
    | Return rs -> rs.token.literal
    | Expression es -> es.token.literal

  let to_string (stmt : t) : string =
    let let_stmt_to_string (s : let_stmt) : string =
      Printf.sprintf "%s %s = %s;" s.token.literal
        (Identifier.to_string s.name)
        (Expression.to_string s.value)
    in
    let return_stmt_to_string (s : return_stmt) : string =
      Printf.sprintf "%s %s;" s.token.literal (Expression.to_string s.value)
    in
    let expr_stmt_to_string (s : expr_stmt) : string =
      Printf.sprintf "%s %s;" s.token.literal
        (Expression.to_string s.expression)
    in
    match stmt with
    | Let ls -> let_stmt_to_string ls
    | Return rs -> return_stmt_to_string rs
    | Expression es -> expr_stmt_to_string es
end
