open Alcotest

let test_empty_statement () =
  let open Monkey in
  let lexer = Lexer.create "" in
  let parser = Parser.create lexer in
  let program, _errors = Parser.parse_program parser in
  check bool "program is empty" true (program = [])

let test_let_statements () =
  let open Monkey in
  let input = "let x = 5; let y = 10; let foobar = 838383;" in
  let expected_ident = [ "x"; "y"; "foobar" ] in
  let lexer = Lexer.create input in
  let parser = Parser.create lexer in
  let program, errors = Parser.parse_program parser in
  check int "number of errors" 0 (List.length errors);
  check int "number of statements" 3 (List.length program);
  let lst = List.combine expected_ident program in
  List.iter
    (fun ((e : string), (s : Stmt.t)) ->
      match s with
      | Let (l : Stmt.let_stmt) ->
          check string "token" "let" (Stmt.token_literal s);
          check string "identifier" e l.name.value
      | _ -> fail "We are only expecting LET statement")
    lst

let test_return_statements () =
  let open Monkey in
  let input = "return 5; return 10; return 838383;" in
  let lexer = Lexer.create input in
  let parser = Parser.create lexer in
  let program, errors = Parser.parse_program parser in
  check int "number of errors" 0 (List.length errors);
  check int "number of statements" 3 (List.length program)

let test_ident_expression () =
  let open Monkey in
  let input = "foobar;" in
  let lexer = Lexer.create input in
  let parser = Parser.create lexer in
  let program, errors = Parser.parse_program parser in
  check int "number of errors" 0 (List.length errors);
  check int "number of statements" 1 (List.length program);
  let stmt = List.hd program in
  match stmt with
  | Expression _e -> check string "todo" "todo" "todo"
  | _ -> fail "We are only expecting EXPR statement"

let test_program_string () =
  let open Monkey in
  let prog : Stmt.t list =
    [
      Ast.Statement.Let
        {
          token = { ty = Token.Type.Let; literal = "let" }
        ; name =
            {
              token = { ty = Token.Type.Ident; literal = "myVar" }
            ; value = "myVar"
            }
        ; value =
            Ident
              {
                token = { ty = Token.Type.Ident; literal = "anotherValue" }
              ; value = "anotherValue"
              }
        }
    ]
  in
  check string "program" "let myVar = anotherValue;"
    (Parser.program_to_string prog)

let tests =
  [
    test_case "Empty statement" `Quick test_empty_statement
  ; test_case "Let statements" `Quick test_let_statements
  ; test_case "Return statements" `Quick test_return_statements
  ; test_case "Expression identifier" `Quick test_ident_expression
  ; test_case "Program string" `Quick test_program_string
  ]
