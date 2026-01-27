module Identifier = struct
  type t = { token : Token.t; value : string }
  (** [identifier] represents a variable name in Monkey. It consists of the
      token associated with the identifier (used for debugging/testing) and the
      string value of the identifier itself. *)
end

module Expression = struct
  type t = unit
  (** [expression] represents expressions in Monkey. For now, the only
      expression is an identifier. Later, this will be extended to include
      literals, function calls, infix expressions, etc. *)
end

module Statement = struct
  type let_stmt = { token : Token.t; name : Identifier.t; value : Expression.t }
  type return_stmt = { token : Token.t; value : Expression.t }

  (** [t] represents all statements in Monkey. Currently, the only statement
      type is 'let_statement', but more can be added later. *)
  type t = Let of let_stmt | Return of return_stmt

  (** [token_literal node] returns the literal value of the token associated
      with the node. This is primarily used for debugging and testing, not for
      evaluation. *)
  let token_literal (stmt : t) : string =
    match stmt with
    | Let ls -> ls.token.literal
    | Return rs -> rs.token.literal
end
