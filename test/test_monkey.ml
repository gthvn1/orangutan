let () =
  let open Alcotest in
  run "MonkeyLang"
    [ ("Lexer", Test_lexer.tests); ("Parser", Test_parser.tests) ]
