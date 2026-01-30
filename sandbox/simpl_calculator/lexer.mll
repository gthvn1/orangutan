{ (* header: optional *)
  type token = EOF
}

rule tokenize =
  parse eof {EOF}

{ (* trailer: optional *) }

