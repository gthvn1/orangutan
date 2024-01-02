type t =
  | Illegal
  | EOF
  | Ident of string
  | Int of int
  | Assign
  | Plus
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Function
  | Let
  | True
  | False
  | If
  | Then
  | Else
  | Return
  | Bang
  | Minus
  | Slash
  | Asterisk
  | LT
  | GT
  | EQ
  | NotEQ

let to_string (tok : t) : string =
  match tok with
  | Illegal -> "ILLEGAL"
  | EOF -> "EOF"
  | Ident s -> "IDENT(" ^ s ^ ")"
  | Int i -> "INT(" ^ string_of_int i ^ ")"
  | Assign -> "ASSIGN"
  | Plus -> "PLUS"
  | Comma -> "COMMA"
  | Semicolon -> "SEMICOLON"
  | LParen -> "LPAREN"
  | RParen -> "RPAREN"
  | LBrace -> "LBRACE"
  | RBrace -> "RBRACE"
  | Function -> "FN"
  | Let -> "LET"
  | True -> "TRUE"
  | False -> "FALSE"
  | If -> "IF"
  | Then -> "THEN"
  | Else -> "ELSE"
  | Return -> "RETURN"
  | Bang -> "BANG"
  | Minus -> "MINUS"
  | Slash -> "SLASH"
  | Asterisk -> "ASTERISK"
  | LT -> "LT"
  | GT -> "GT"
  | EQ -> "EQ"
  | NotEQ -> "NotEQ"

let pp (f : Format.formatter) (tok : t) : unit =
  Format.fprintf f "%s" (to_string tok)
