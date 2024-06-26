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

let peek_char (lexer : t) : char =
  if lexer.read_position >= String.length lexer.input then '\000'
  else String.get lexer.input lexer.read_position

let rec skip_whitespace (lexer : t) : t =
  match lexer.ch with
  | ' ' | '\t' | '\n' | '\r' -> skip_whitespace (read_char lexer)
  | _ -> lexer

let is_letter (ch : char) : bool =
  ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch = '_'

let is_digit (ch : char) : bool = '0' <= ch && ch <= '9'

let read_identifier (lexer : t) : t * Token.t =
  let start_pos = lexer.read_position - 1 in
  let rec aux (lexer : t) : t =
    if is_letter lexer.ch then aux (read_char lexer) else lexer
  in
  let lexer = aux lexer in
  let ident =
    String.sub lexer.input start_pos (lexer.read_position - 1 - start_pos)
  in
  match ident with
  | "let" -> (lexer, Token.Let)
  | "fn" -> (lexer, Token.Function)
  | "true" -> (lexer, Token.True)
  | "false" -> (lexer, Token.False)
  | "if" -> (lexer, Token.If)
  | "then" -> (lexer, Token.Then)
  | "else" -> (lexer, Token.Else)
  | "return" -> (lexer, Token.Return)
  | _ -> (lexer, Token.Ident ident)

let read_number (lexer : t) : t * Token.t =
  let start_pos = lexer.read_position - 1 in
  let rec aux (lexer : t) : t =
    if is_digit lexer.ch then aux (read_char lexer) else lexer
  in
  let lexer = aux lexer in
  let num =
    String.sub lexer.input start_pos (lexer.read_position - 1 - start_pos)
  in
  (lexer, Token.Int (int_of_string num))

(** We want to provide a Sequence of Tokens *)
let rec tokens (lexer : t) : Token.t Seq.t =
  let lexer = skip_whitespace lexer in
  let lexer, tok =
    match lexer.ch with
    | '=' ->
        if peek_char lexer = '=' then (read_char lexer |> read_char, Token.EQ)
        else (read_char lexer, Token.Assign)
    | '!' ->
        if peek_char lexer = '=' then (read_char lexer |> read_char, Token.NotEQ)
        else (read_char lexer, Token.Bang)
    | ';' -> (read_char lexer, Token.Semicolon)
    | ',' -> (read_char lexer, Token.Comma)
    | '(' -> (read_char lexer, Token.LParen)
    | ')' -> (read_char lexer, Token.RParen)
    | '+' -> (read_char lexer, Token.Plus)
    | '-' -> (read_char lexer, Token.Minus)
    | '*' -> (read_char lexer, Token.Asterisk)
    | '/' -> (read_char lexer, Token.Slash)
    | '<' -> (read_char lexer, Token.LT)
    | '>' -> (read_char lexer, Token.GT)
    | '{' -> (read_char lexer, Token.LBrace)
    | '}' -> (read_char lexer, Token.RBrace)
    | '\000' -> (read_char lexer, Token.EOF)
    | _ ->
        if is_letter lexer.ch then read_identifier lexer
        else if is_digit lexer.ch then read_number lexer
        else (read_char lexer, Token.Illegal)
  in
  fun () -> Seq.Cons (tok, tokens lexer)

let tokens_hd (toks : Token.t Seq.t) : Token.t =
  match toks () with Seq.Cons (tok, _) -> tok | Nil -> Token.EOF

let tokens_tl (toks : Token.t Seq.t) : Token.t Seq.t =
  match toks () with Seq.Cons (_, tl) -> tl | Nil -> Seq.empty

let next_token (toks : Token.t Seq.t) : Token.t * Token.t Seq.t =
  (tokens_hd toks, tokens_tl toks)
