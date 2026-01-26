let () =
  let open Alcotest in
  run "Monkey test"
    [ ("Lexer", Test_lexer.tests); ("Parser", Test_parser.tests) ]
