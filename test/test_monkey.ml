let () =
  let open Alcotest in
  run "Monkey test" [ ("test lexer", Test_lexer.tests) ]
