type t = { input : string; read_position : int; ch : char }

let new_lexer (str : string) : t =
  {
    input = str;
    (* current position *)
    read_position = 1;
    (* after current char, the reading position *)
    ch = String.get str 0;
    (* current char *)
  }

let read_char (lexer : t) : t =
  if lexer.read_position >= String.length lexer.input then
    { lexer with ch = '\000' }
  else
    {
      lexer with
      ch = String.get lexer.input lexer.read_position;
      read_position = lexer.read_position + 1;
    }

let rec skip_whitespace (lexer : t) : t =
  match lexer.ch with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (read_char lexer)
  | _ -> lexer

(** Identifier supports ascii and '_' *)
let is_letter (ch : char) : bool =
  ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_'

let read_identifier (lexer : t) : t * string =
  let _ = is_letter lexer.ch in
  (* added to use lexer *)
  failwith "TODO"

let read_number (lexer : t) : t * int =
  let _ = is_letter lexer.ch in
  (* added to use lexer *)
  failwith "TODO"

(** We want to provide a Sequence of Tokens *)
let rec next_token (lexer : t) : Token.t Seq.t =
  let lexer = skip_whitespace lexer in
  let tok =
    match lexer.ch with
    | '=' -> Token.Assign
    | ';' -> Token.Semicolon
    | '(' -> Token.LParen
    | ')' -> Token.RParen
    | ',' -> Token.Comma
    | '+' -> Token.Plus
    | '{' -> Token.LBrace
    | '}' -> Token.RBrace
    | '\000' -> Token.EOF
    | _ -> Token.Illegal
  in
  fun () -> Seq.Cons (tok, next_token (read_char lexer))
