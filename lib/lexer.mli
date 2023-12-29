type t

val new_lexer : string -> t
(** [new_lexer input] creates a new lexer with the given input string. *)

val read_char : t -> t
(** [read_char l] reads the next character in the input string and updates the
    lexer accordingly. *)
