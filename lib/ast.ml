type identifier = { token : Token.t; value : string }
(** [identifier] represents a variable name in Monkey. It consists of the token
    associated with the identifier (used for debugging/testing) and the string
    value of the identifier itself. *)

(** [e] represents expressions in Monkey. For now, the only expression is an
    identifier. Later, this will be extended to include literals, function
    calls, infix expressions, etc. *)
type e = Ident of identifier

type let_statement = { token : Token.t; name : identifier; value : e }
(** [let_statement] represents a 'let' statement in Monkey. It binds an
    expression to a variable name.

    Fields:
    - [token]: the 'let' token itself (used for token_literal, debugging)
    - [name]: the identifier being bound
    - [value]: the expression assigned to the identifier *)

(** [s] represents all statements in Monkey. Currently, the only statement type
    is 'let_statement', but more can be added later. *)
type s = Let of let_statement

type program = s list
(** [program] is the root of every AST. It is simply a list of statements,
    representing the Monkey program. *)

(** [node] is a wrapper type to allow a single function (like [token_literal])
    to operate on both expressions and statements.

    - [Expression e]: wraps an expression
    - [Statement s]: wraps a statement *)
type node = Expression of e | Statement of s

(** [token_literal node] returns the literal value of the token associated with
    the node. This is primarily used for debugging and testing, not for
    evaluation. *)
let token_literal node : string =
  match node with
  | Statement (Let l) -> l.token.literal
  | Expression (Ident i) -> i.token.literal
