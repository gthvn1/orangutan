open Alcotest

let test_empty_statement () =
  let open Monkey in
  let lexer = Lexer.create "" in
  let parser = Parser.create lexer in
  let program = Parser.parse_program parser in
  check bool "program is empty" true (program = [])

let tests = [ test_case "Empty statement" `Quick test_empty_statement ]
