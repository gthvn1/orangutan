type token_type =
  | Illegal
  | Eof
  (* Identifiers + literals *)
  | Ident
  | Int
  (* Operators *)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Lt
  | Gt
  | Eq
  | NotEq
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
  | True
  | False
  | If
  | Else
  | Return

type t = { ty : token_type; literal : string }

let lookup_ident (str : string) : token_type =
  match str with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Ident

let string_of_token_type = function
  | Illegal -> "ILLEGAL"
  | Eof -> "EOF"
  | Ident -> "IDENT"
  | Int -> "INT"
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Minus -> "MINUS"
  | Bang -> "BANG"
  | Asterisk -> "ASTERISK"
  | Slash -> "SLASH"
  | Lt -> "LESSTHAN"
  | Gt -> "GREATERTHAN"
  | Eq -> "EQUAL"
  | NotEq -> "NOTEQUAL"
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | Lparen -> "LPAREN"
  | Rparen -> "RPAREN"
  | Lbrace -> "LBRACE"
  | Rbrace -> "RBRACE"
  | Function -> "FUNCTION"
  | Let -> "LET"
  | True -> "TRUE"
  | False -> "FALSE"
  | If -> "IF"
  | Else -> "ELSE"
  | Return -> "RETURN"

let string_of_token (t : t) : string =
  Printf.sprintf "Token(%s, %S)" (string_of_token_type t.ty) t.literal
