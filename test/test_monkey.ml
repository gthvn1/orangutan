let pp_token_type fmt tt =
  let open Monkey in
  Format.fprintf fmt "%s"
    (match tt with
    | Assign -> "Assign"
    | Plus -> "Plus"
    | Lparen -> "Lparen"
    | Rparen -> "Rparen"
    | Lbrace -> "Lbrace"
    | Rbrace -> "Rbrace"
    | Comma -> "Comma"
    | Semicolon -> "Semicolon"
    | Eof -> "Eof")

let token_type = Alcotest.testable pp_token_type ( = )

let test_create_lexer () =
  let open Alcotest in
  let new_lexer = Monkey.Lexer.create "abc" in
  check string "input" "abc" new_lexer.input;
  check int "position" 0 new_lexer.position;
  check int "read position" 0 new_lexer.read_position;
  check char "ch" '\000' new_lexer.ch

let test_read_char () =
  let open Monkey in
  let open Alcotest in
  let read_and_check l ~str ~car =
    let new_lexer = Lexer.read_char l in
    check char str car new_lexer.ch;
    new_lexer
  in
  Lexer.create "abc"
  |> read_and_check ~str:"first read" ~car:'a'
  |> read_and_check ~str:"second read" ~car:'b'
  |> read_and_check ~str:"third read" ~car:'c'
  |> read_and_check ~str:"fourth read" ~car:'\000'
  |> read_and_check ~str:"fifth read" ~car:'\000'
  |> ignore

let _test_next_token () =
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
    [
      ("create-lexer", [ test_case "Create a lexer" `Quick test_create_lexer ])
    ; ("read-char", [ test_case "Read chars" `Quick test_read_char ])
      (* ("next-token", [ test_case "Simple tokens" `Quick test_next_token ]) ;
*)
    ]
