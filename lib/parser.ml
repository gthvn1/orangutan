module L = Lexer
module T = Token

type t = { l : L.t; cur_token : T.t; peek_token : T.t }

let new_parser (l : L.t) : t =
  { l; cur_token = T.Illegal; peek_token = T.Illegal }
