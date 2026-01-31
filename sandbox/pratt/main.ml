let () =
  let open Pratt.Lexer in
  let _p : Token.t = Token.Plus in
  tokenize "123" |> ignore;
  print_endline "Hello from pratt"
