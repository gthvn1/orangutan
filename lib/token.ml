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

let lookup_ident (str : string) : token_type =
  match str with
  | "fn" -> Function
  | "let" -> Let
  | _ -> Ident

let string_of_token_type = function
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  | Ident -> "IDENT"
  | Int -> "INT"
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | Lparen -> "LPAREN"
  | Rparen -> "RPAREN"
  | Lbrace -> "LBRACE"
  | Rbrace -> "RBRACE"
  | Function -> "FUNCTION"
  | Let -> "LET"

let string_of_token (t : t) : string =
  Printf.sprintf "Token(%s, %S)" (string_of_token_type t.ty) t.literal
