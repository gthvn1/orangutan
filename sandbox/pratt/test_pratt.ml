let token_testable =
  let open Pratt.Lexer in
  let token_str = Token.to_string in
  let pp fmt token = Format.pp_print_string fmt (token_str token) in
  Alcotest.testable pp ( = )

let input_tokens = "-13-(+0123)"

let test_tokens () =
  let open Pratt.Lexer in
  let expected =
    [
      Token.Minus
    ; Token.Int 13
    ; Token.Minus
    ; Token.Lparen
    ; Token.Plus
    ; Token.Int 123
    ; Token.Rparen
    ]
  in
  Alcotest.(check (list token_testable))
    "some tokens" expected (tokenize input_tokens)

(* Run it *)
let () =
  let open Alcotest in
  run "Pratt" [ ("some-tokens", [ test_case input_tokens `Slow test_tokens ]) ]
