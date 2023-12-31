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

let pp (f : Format.formatter) (tok : t) : unit =
  match tok with
  | Illegal -> Format.fprintf f "ILLEGAL"
  | EOF -> Format.fprintf f "EOF"
  | Ident s -> Format.fprintf f "IDENT(%s)" s
  | Int i -> Format.fprintf f "INT(%d)" i
  | Assign -> Format.fprintf f "ASSIGN"
  | Plus -> Format.fprintf f "PLUS"
  | Comma -> Format.fprintf f "COMMA"
  | Semicolon -> Format.fprintf f "SEMICOLON"
  | LParen -> Format.fprintf f "LPAREN"
  | RParen -> Format.fprintf f "RPAREN"
  | LBrace -> Format.fprintf f "LBRACE"
  | RBrace -> Format.fprintf f "RBRACE"
  | Function -> Format.fprintf f "FN"
  | Let -> Format.fprintf f "LET"
  | True -> Format.fprintf f "TRUE"
  | False -> Format.fprintf f "FALSE"
  | If -> Format.fprintf f "IF"
  | Then -> Format.fprintf f "THEN"
  | Else -> Format.fprintf f "ELSE"
  | Return -> Format.fprintf f "RETURN"
  | Bang -> Format.fprintf f "BANG"
  | Minus -> Format.fprintf f "MINUS"
  | Slash -> Format.fprintf f "SLASH"
  | Asterisk -> Format.fprintf f "ASTERISK"
  | LT -> Format.fprintf f "LT"
  | GT -> Format.fprintf f "GT"
  | EQ -> Format.fprintf f "EQ"
  | NotEQ -> Format.fprintf f "NotEQ"
