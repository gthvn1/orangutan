let token_testable =
  let token_str = Pratt.Token.to_string in
  let pp fmt token = Format.pp_print_string fmt (token_str token) in
  Alcotest.testable pp ( = )

let input_tokens = "-13-(+0123)"

let test_tokens () =
  let expected =
    [
      Pratt.Token.Minus
    ; Pratt.Token.Int 13
    ; Pratt.Token.Minus
    ; Pratt.Token.Lparen
    ; Pratt.Token.Plus
    ; Pratt.Token.Int 123
    ; Pratt.Token.Rparen
    ]
  in
  Alcotest.(check (list token_testable))
    "some tokens" expected
    (Pratt.to_tokens input_tokens)

(* Run it *)
let () =
  let open Alcotest in
  run "Pratt" [ ("some-tokens", [ test_case input_tokens `Slow test_tokens ]) ]
