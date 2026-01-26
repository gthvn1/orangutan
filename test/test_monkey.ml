let () =
  let open Alcotest in
  run "Monkey test"
    [ ("test lexer", Test_lexer.tests); ("test parser", Test_parser.tests) ]
