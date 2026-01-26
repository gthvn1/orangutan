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
  type let_statement = {
      token : Token.t
    ; name : Identifier.t
    ; value : Expression.t
  }
  (** [let_statement] represents a 'let' statement in Monkey. It binds an
      expression to a variable name.

      Fields:
      - [token]: the 'let' token itself (used for token_literal, debugging)
      - [name]: the identifier being bound
      - [value]: the expression assigned to the identifier *)

  (** [statement] represents all statements in Monkey. Currently, the only
      statement type is 'let_statement', but more can be added later. *)
  type t = Let of let_statement

  (** [token_literal node] returns the literal value of the token associated
      with the node. This is primarily used for debugging and testing, not for
      evaluation. *)
  let token_literal (stmt : t) : string =
    match stmt with
    | Let l -> l.token.literal
end
