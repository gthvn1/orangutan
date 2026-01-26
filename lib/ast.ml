type identifier = { token : Token.t; value : string }
(** [identifier] represents a variable name in Monkey. It consists of the token
    associated with the identifier (used for debugging/testing) and the string
    value of the identifier itself. *)

(** [expression] represents expressions in Monkey. For now, the only expression
    is an identifier. Later, this will be extended to include literals, function
    calls, infix expressions, etc. *)
type expression = Ident of identifier

type let_statement = { token : Token.t; name : identifier; value : expression }
(** [let_statement] represents a 'let' statement in Monkey. It binds an
    expression to a variable name.

    Fields:
    - [token]: the 'let' token itself (used for token_literal, debugging)
    - [name]: the identifier being bound
    - [value]: the expression assigned to the identifier *)

(** [statement] represents all statements in Monkey. Currently, the only
    statement type is 'let_statement', but more can be added later. *)
type statement = Let of let_statement

(** [node] is a wrapper type to allow a single function (like [token_literal])
    to operate on both expressions and statements.

    - [E expression]: wraps an expression
    - [S statement]: wraps a statement *)
type node = E of expression | S of statement

(** [token_literal node] returns the literal value of the token associated with
    the node. This is primarily used for debugging and testing, not for
    evaluation. *)
let token_literal node : string =
  match node with
  | S (Let l) -> l.token.literal
  | E (Ident i) -> i.token.literal
