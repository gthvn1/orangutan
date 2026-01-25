(** [is_letter ch] returns true if [ch] is a letter. '_' is a valid letter. It
    returns false otherwise. *)
let is_letter ch =
  ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_'

type t = {
    input : string
  ; position : int
        (** current position in input (points to the current char) *)
  ; read_position : int
        (** current reading position in input (after current char) *)
  ; ch : char  (** current char under examination *)
}

(** [read_char lexer] returns a new lexer where field "ch" is set with the new
    character. The new lexer has the new position. *)
let read_char (lexer : t) : t =
  if lexer.read_position >= String.length lexer.input then
    { lexer with ch = '\000' }
  else
    {
      lexer with
      position = lexer.read_position
    ; read_position = lexer.read_position + 1
    ; ch = lexer.input.[lexer.read_position]
    }

(** [create input] returns a lexer initialized with string [input]. It returns a
    fully working state. *)
let create (input : string) : t =
  let init_state = { input; position = 0; read_position = 0; ch = '\000' } in
  read_char init_state

(** [read_identifier lexer] returns the indentifer as a string and the new
    lexer. It raises an exception if something goes wrong. *)
let read_identifier (_lexer : t) : string * t =
  failwith "TODO: implement read identifier"

(** [next_token lexer] returns a tuple that is the token found and the new
    lexer. It raises an expection if something goes wrong. *)
let next_token (lexer : t) : Token.t * t =
  let new_token (tt : Token.token_type) : Token.t =
    { ty = tt; literal = String.make 1 lexer.ch }
  in
  match lexer.ch with
  | '=' -> (new_token Assign, read_char lexer)
  | ';' -> (new_token Semicolon, read_char lexer)
  | '(' -> (new_token Lparen, read_char lexer)
  | ')' -> (new_token Rparen, read_char lexer)
  | ',' -> (new_token Comma, read_char lexer)
  | '+' -> (new_token Plus, read_char lexer)
  | '{' -> (new_token Lbrace, read_char lexer)
  | '}' -> (new_token Rbrace, read_char lexer)
  | '\000' -> ({ ty = Eof; literal = "" }, lexer)
  | c ->
      if is_letter c then
        let literal, new_lexer = read_identifier lexer in
        ({ ty = Ident; literal }, new_lexer)
      else (
        Printf.eprintf "Error: char not recognized";
        (new_token Illegal, lexer))
