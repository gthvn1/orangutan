let pp_token_type fmt tt =
  Format.fprintf fmt "%s" (Monkey.Token.string_of_token_type tt)

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
    let next_lexer = Lexer.read_char l in
    check char str car next_lexer.ch;
    next_lexer
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
  let open Monkey.Token in
  let input = "=+(){},;" in
  let expected =
    [
      (Assign, "=")
    ; (Plus, "+")
    ; (Lparen, "(")
    ; (Rparen, ")")
    ; (Lbrace, "{")
    ; (Rbrace, "}")
    ; (Comma, ",")
    ; (Semicolon, ";")
    ; (Eof, "")
    ]
  in
  let lexer = Monkey.Lexer.create input in
  let rec aux l e =
    let tok, next_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux next_lexer xs
  in
  aux lexer expected

let test_next_simple_tokens_with_spaces () =
  let open Monkey.Token in
  let input = " =\t+(){},;" in
  let expected =
    [
      (Assign, "=")
    ; (Plus, "+")
    ; (Lparen, "(")
    ; (Rparen, ")")
    ; (Lbrace, "{")
    ; (Rbrace, "}")
    ; (Comma, ",")
    ; (Semicolon, ";")
    ; (Eof, "")
    ]
  in
  let lexer = Monkey.Lexer.create input in
  let rec aux l e =
    let tok, next_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Printf.printf "got %s\n" (string_of_token tok);
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux next_lexer xs
  in
  aux lexer expected

let test_next_one_let () =
  let open Monkey.Token in
  let input = "let a = b" in
  let expected =
    [ (Let, "let"); (Ident, "a"); (Assign, "="); (Ident, "b"); (Eof, "") ]
  in
  let lexer = Monkey.Lexer.create input in
  let rec aux l e =
    let tok, next_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux next_lexer xs
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
  let open Monkey.Token in
  let expected =
    [
      (Let, "let")
    ; (Ident, "five")
    ; (Assign, "=")
    ; (Int, "5")
    ; (Semicolon, ";")
    ; (Let, "let")
    ; (Ident, "ten")
    ; (Assign, "=")
    ; (Int, "10")
    ; (Semicolon, ";")
    ; (Let, "let")
    ; (Ident, "add")
    ; (Assign, "=")
    ; (Function, "fn")
    ; (Lparen, "(")
    ; (Ident, "x")
    ; (Comma, ",")
    ; (Ident, "y")
    ; (Rparen, ")")
    ; (Lbrace, "{")
    ; (Ident, "x")
    ; (Plus, "+")
    ; (Ident, "y")
    ; (Semicolon, ";")
    ; (Rbrace, "}")
    ; (Semicolon, ";")
    ; (Let, "let")
    ; (Ident, "result")
    ; (Assign, "=")
    ; (Ident, "add")
    ; (Lparen, "(")
    ; (Ident, "five")
    ; (Comma, ",")
    ; (Ident, "ten")
    ; (Rparen, ")")
    ; (Semicolon, ";")
    ; (Eof, "")
    ]
  in
  let lexer = Monkey.Lexer.create input in
  let rec aux l e =
    let tok, next_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux next_lexer xs
  in
  aux lexer expected

let test_next_medium_program () =
  let input =
    {|
let five = 5;
let ten = 10;
let add = fn(x, y) {
x + y;
};
let result = add(five, ten);
!-/*5;
5 < 10 > 5;
|}
  in
  let open Monkey.Token in
  let expected =
    [
      (Let, "let")
    ; (Ident, "five")
    ; (Assign, "=")
    ; (Int, "5")
    ; (Semicolon, ";")
    ; (Let, "let")
    ; (Ident, "ten")
    ; (Assign, "=")
    ; (Int, "10")
    ; (Semicolon, ";")
    ; (Let, "let")
    ; (Ident, "add")
    ; (Assign, "=")
    ; (Function, "fn")
    ; (Lparen, "(")
    ; (Ident, "x")
    ; (Comma, ",")
    ; (Ident, "y")
    ; (Rparen, ")")
    ; (Lbrace, "{")
    ; (Ident, "x")
    ; (Plus, "+")
    ; (Ident, "y")
    ; (Semicolon, ";")
    ; (Rbrace, "}")
    ; (Semicolon, ";")
    ; (Let, "let")
    ; (Ident, "result")
    ; (Assign, "=")
    ; (Ident, "add")
    ; (Lparen, "(")
    ; (Ident, "five")
    ; (Comma, ",")
    ; (Ident, "ten")
    ; (Rparen, ")")
    ; (Semicolon, ";")
    ; (* !-/*5; *)
      (Bang, "!")
    ; (Minus, "-")
    ; (Slash, "/")
    ; (Asterisk, "*")
    ; (Int, "5")
    ; (Semicolon, ";")
    ; (* 5 < 10 > 5; *)
      (Int, "5")
    ; (Lt, "<")
    ; (Int, "10")
    ; (Gt, ">")
    ; (Int, "5")
    ; (Semicolon, ";")
    ; (Eof, "")
    ]
  in
  let lexer = Monkey.Lexer.create input in
  let rec aux l e =
    let tok, next_lexer = Monkey.Lexer.next_token l in
    match e with
    | [] -> ()
    | (tt, lit) :: xs ->
        Alcotest.check token_type "same token type" tt tok.ty;
        Alcotest.check Alcotest.string "same literal" lit tok.literal;
        aux next_lexer xs
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
        ; test_case "Simple tokens with spaces" `Quick
            test_next_simple_tokens_with_spaces
        ; test_case "One let" `Quick test_next_one_let
        ; test_case "Less Simple tokens" `Quick test_next_less_simple_tokens
        ; test_case "Medium program" `Quick test_next_medium_program
        ] )
    ]
