let check_token (token : Calc_lexer.token) : unit =
  match token with
  | Calc_lexer.Plus -> print_endline "Got plus"
  | Calc_lexer.Minus -> print_endline "Got minus"
  | Calc_lexer.Lparen -> print_endline "Got lparen"
  | Calc_lexer.Rparen -> print_endline "Got rparen"
  | Calc_lexer.Eof -> print_endline "Got end of file"

let () =
  let lexbuf = Lexing.from_string "+-()" in
  Calc_lexer.tokenize lexbuf |> check_token;
  Calc_lexer.tokenize lexbuf |> check_token;
  Calc_lexer.tokenize lexbuf |> check_token;
  Calc_lexer.tokenize lexbuf |> check_token;
  Calc_lexer.tokenize lexbuf |> check_token;
  Calc_lexer.tokenize lexbuf |> check_token
