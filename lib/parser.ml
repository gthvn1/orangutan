module L = Lexer
module T = Token

type t = {
  l : L.t;
  cur_token: T.t;
  peek_token : T.t;
}
