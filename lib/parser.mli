module A = Ast
module L = Lexer
module T = Token

type t = T.t * T.t Seq.t

val new_parser : L.t -> t
val next_token : t -> t
val peek_token : t -> T.t
val parse_program : t -> A.Program.t
