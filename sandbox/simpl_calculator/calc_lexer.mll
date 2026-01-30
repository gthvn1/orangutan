{ (* header: optional *)
  type token = Eof | Plus | Minus | Lparen | Rparen
}

rule tokenize =
  parse
  | "+" { Plus }
  | "-" { Minus }
  | "(" { Lparen }
  | ")" { Rparen }
  | eof { Eof }

{ (* trailer: optional *) }

