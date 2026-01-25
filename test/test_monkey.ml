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
    | Eof -> "Eof"
    | _ -> failwith "Need to add the token")

let token_type = Alcotest.testable pp_token_type ( = )

let test_create_lexer () =
  let open Alcotest in
  let input = "hello" in
  let new_lexer = Monkey.Lexer.create input in
  check string "input" input new_lexer.input;
  check int "position" 0 new_lexer.position;
  check int "read position" 1 new_lexer.read_position;
  check char "ch" 'h' new_lexer.ch

let test_read_char () =
  let open Monkey in
  let open Alcotest in
  let read_and_check l ~str ~car =
    let new_lexer = Lexer.read_char l in
    check char str car new_lexer.ch;
    new_lexer
  in
  let input = "abc" in
  (let l = Lexer.create input in
   (* Here l.ch should be set to 'a' and read position is point to b. It is
     the first valid state already checked in test_create_lexer. So next
     read should give 'b', next should give 'c' and so on *)
   check string "input" input l.input;
   check int "position" 0 l.position;
   check int "read position" 1 l.read_position;
   check char "ch" 'a' l.ch;
   read_and_check ~str:"next read" ~car:'b' l)
  |> read_and_check ~str:"next read" ~car:'c'
  |> read_and_check ~str:"next read" ~car:'\000'
  |> read_and_check ~str:"next read" ~car:'\000'
  |> ignore

let test_next_simple_tokens () =
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
  let rec aux l e =
    let tok, new_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux new_lexer xs
  in
  aux lexer expected

let test_next_less_simple_tokens () =
  let input =
    {|
let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five,ten);
|}
  in
  let expected =
    [
      (Monkey.Let, "let")
    ; (Monkey.Ident, "five")
    ; (Monkey.Assign, "=")
    ; (Monkey.Int, "5")
    ; (Monkey.Semicolon, ";")
    ; (Monkey.Let, "let")
    ; (Monkey.Ident, "ten")
    ; (Monkey.Assign, "=")
    ; (Monkey.Int, "10")
    ; (Monkey.Semicolon, ";")
    ; (Monkey.Let, "let")
    ; (Monkey.Ident, "add")
    ; (Monkey.Assign, "=")
    ; (Monkey.Function, "fn")
    ; (Monkey.Lparen, "(")
    ; (Monkey.Ident, "x")
    ; (Monkey.Comma, ",")
    ; (Monkey.Ident, "y")
    ; (Monkey.Rparen, ")")
    ; (Monkey.Lbrace, "{")
    ; (Monkey.Ident, "x")
    ; (Monkey.Plus, "+")
    ; (Monkey.Ident, "y")
    ; (Monkey.Semicolon, ";")
    ; (Monkey.Rbrace, "}")
    ; (Monkey.Semicolon, ";")
    ; (Monkey.Let, "let")
    ; (Monkey.Ident, "result")
    ; (Monkey.Assign, "=")
    ; (Monkey.Ident, "add")
    ; (Monkey.Lparen, "(")
    ; (Monkey.Ident, "five")
    ; (Monkey.Comma, ",")
    ; (Monkey.Ident, "ten")
    ; (Monkey.Rparen, ")")
    ; (Monkey.Semicolon, ";")
    ; (Monkey.Eof, "")
    ]
  in
  let lexer = Monkey.Lexer.create input in
  let rec aux l e =
    let tok, new_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux new_lexer xs
  in
  aux lexer expected

let () =
  let open Alcotest in
  run "Monkey test"
    [
      ("create-lexer", [ test_case "Create a lexer" `Quick test_create_lexer ])
    ; ("read-char", [ test_case "Read chars" `Quick test_read_char ])
    ; ( "next-token"
      , [
          test_case "Simple tokens" `Quick test_next_simple_tokens
        ; test_case "Less Simple tokens" `Quick test_next_less_simple_tokens
        ] )
    ]
