type t = { input : string; read_position : int; ch : char }

val new_lexer : string -> t
(** [new_lexer input] creates a new lexer with the given input string. *)

val read_char : t -> t
(** [read_char l] reads the next character in the input string and return the
    updated new lexer. *)

val next_token : t -> Token.t Seq.t
(** [next_token lexer] returns a sequence of tokens for a given [lexer]. *)
