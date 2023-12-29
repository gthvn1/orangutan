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
