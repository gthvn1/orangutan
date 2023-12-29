type t

val new_lexer : string -> t
(** [new_lexer input] creates a new lexer with the given input string. *)

val read_char : t -> unit
(** [read_char l] reads the next character in the input string and updates the
    lexer accordingly. *)

val next_token : t -> Token.t
(** [next_token l] returns the next token in the input string. *)
