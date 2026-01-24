let pp_token_type fmt tt =
  Format.fprintf fmt "%s"
    (match tt with
    | Monkey.Assign -> "Assign"
    | Monkey.Plus -> "Plus"
    | Monkey.Lparen -> "Lparen"
    | Monkey.Rparen -> "Rparen"
    | Monkey.Lbrace -> "Lbrace"
    | Monkey.Rbrace -> "Rbrace"
    | Monkey.Comma -> "Comma"
    | Monkey.Semicolon -> "Semicolon"
    | Monkey.Eof -> "Eof")

let token_type = Alcotest.testable pp_token_type ( = )

let test_next_token () =
  let input = "=+(){},;" in
  let expected =
    [
      (Monkey.Assign, "=")
    ; (Monkey.Plus, "+")
    ; (Monkey.Lparen, "(")
    ; (Monkey.Rparen, ")")
    ; (Monkey.Lbrace, "{")
    ; (Monkey.Rbrace, "}")
    ; (Monkey.Comma, ",")
    ; (Monkey.Semicolon, ";")
    ; (Monkey.Eof, "")
    ]
  in
  let lexer = Monkey.Lexer.create input in
  List.iter
    (fun (tt, lit) ->
      let tok = Monkey.Lexer.next_token lexer in
      Alcotest.check token_type "same token type" tt tok.ty;
      Alcotest.check Alcotest.string "same literal" lit tok.literal)
    expected

let () =
  let open Alcotest in
  run "Monkey test"
    [ ("next-token", [ test_case "Simple tokens" `Quick test_next_token ]) ]
