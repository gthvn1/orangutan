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

let pp (_f : Format.formatter) (tok : t) : unit =
  match tok with
  | Illegal -> Format.fprintf _f "ILLEGAL"
  | EOF -> Format.fprintf _f "EOF"
  | Ident s -> Format.fprintf _f "Ident(%s)" s
  | Int i -> Format.fprintf _f "Int(%d)" i
  | Assign -> Format.fprintf _f "="
  | Plus -> Format.fprintf _f "+"
  | Comma -> Format.fprintf _f ","
  | Semicolon -> Format.fprintf _f ";"
  | LParen -> Format.fprintf _f "("
  | RParen -> Format.fprintf _f ")"
  | LBrace -> Format.fprintf _f "{"
  | RBrace -> Format.fprintf _f "}"
  | Function -> Format.fprintf _f "fn"
  | Let -> Format.fprintf _f "let"
  | True -> Format.fprintf _f "true"
  | False -> Format.fprintf _f "false"
  | If -> Format.fprintf _f "if"
  | Else -> Format.fprintf _f "else"
  | Return -> Format.fprintf _f "return"
  | Bang -> Format.fprintf _f "!"
  | Minus -> Format.fprintf _f "-"
  | Slash -> Format.fprintf _f "/"
  | Asterisk -> Format.fprintf _f "*"
  | LT -> Format.fprintf _f "<"
  | GT -> Format.fprintf _f ">"
  | EQ -> Format.fprintf _f "=="
  | NotEQ -> Format.fprintf _f "!="
