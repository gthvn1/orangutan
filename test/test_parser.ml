open Alcotest

let test_empty_statement () =
  let open Monkey in
  let lexer = Lexer.create "" in
  let parser = Parser.create lexer in
  let program = Parser.parse_program parser in
  check bool "program is empty" true (program = [])

let test_let_statements () =
  let open Monkey in
  let input = "let x = 5; let y = 10; let foobar = 838383;" in
  let lexer = Lexer.create input in
  let parser = Parser.create lexer in
  let program = Parser.parse_program parser in
  check int "number of statements" 3 (List.length program)

let tests =
  [
    test_case "Empty statement" `Quick test_empty_statement
  ; test_case "Let statements" `Quick test_let_statements
  ]
