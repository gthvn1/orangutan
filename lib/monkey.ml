type token_type =
  | Assign
  | Plus
  | Lparen
  | Rparen
  | Lbrace
  | Rbrace
  | Comma
  | Semicolon
  | Eof

type token = { ty : token_type; literal : string }

module Lexer = struct
  type t = { input : string }

  let create (input : string) : t = { input }
  let next_token (_lexer : t) : token = failwith "todo"
end
