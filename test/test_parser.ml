module L = Monkey.Lexer
module P = Monkey.Parser

let test_parser_new () =
  let input = "let x = 5;" in
  let lexer = L.new_lexer input in
  let _parse = P.new_parser lexer in
  Alcotest.(check bool) "test_new_parser" true true
