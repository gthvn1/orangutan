type t = { input : string; read_position : int; ch : char }

val new_lexer : string -> t
(** [new_lexer input] creates a new lexer with the given input string. *)

val read_char : t -> t
(** [read_char l] reads the next character in the input string and return the
    updated new lexer. *)

val read_identifier : t -> t * Token.t
(** [read_identifier l] reads the identifier in the input string. It returns the
    updated lexer and the corresponding token. It supports ascii character and
    '_'. *)

val read_number : t -> t * Token.t
(** [read_number l] reads the next number in the input string. It returns the
    updated lexer and the Token.Int. *)

val tokens : t -> Token.t Seq.t
(** [tokens lexer] returns a sequence of tokens for a given [lexer]. *)

val tokens_hd : Token.t Seq.t -> Token.t
(** [tokens_hd toks] returns the first token in the sequence of tokens. *)

val next_token : Token.t Seq.t -> Token.t * Token.t Seq.t
(** [next_token toks] returns the next token and the updated sequence of tokens. *)
