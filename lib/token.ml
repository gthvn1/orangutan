type token_type =
  | Illegal
  | Eof
  (* Identifiers + literals *)
  | Ident
  | Int
  (* Operators *)
  | Assign
  | Plus
  (* Delimiters *)
  | Comma
  | Semicolon
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  (* Keywords *)
  | Function
  | Let

type t = { ty : token_type; literal : string }
