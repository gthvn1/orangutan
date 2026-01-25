type token_type =
  | Assign
  | Plus
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Comma
  | Semicolon
  | Eof

type token = { ty : token_type; literal : string }

module Lexer = struct
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

  (** [create input] returns a lexer initialized with string [input]. It returns
      a fully working state. *)
  let create (input : string) : t =
    let init_state = { input; position = 0; read_position = 0; ch = '\000' } in
    read_char init_state

  let next_token (_lexer : t) : token = failwith "todo"
end
