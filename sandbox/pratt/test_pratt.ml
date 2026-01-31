let pp_token fmt token =
  let open Pratt.Lexer in
  Format.pp_print_string fmt (Token.to_string token)

let token_testable = Alcotest.testable pp_token ( = )

(* We need a pretty printer for expr *)
let rec pp_expr fmt = function
  | Pratt.Parser.Int n -> Format.fprintf fmt "Int %d" n
  | Pratt.Parser.Neg e -> Format.fprintf fmt "Neg (%a)" pp_expr e
  | Pratt.Parser.Add (l, r) ->
      Format.fprintf fmt "Add (%a,%a)" pp_expr l pp_expr r
  | Pratt.Parser.Sub (l, r) ->
      Format.fprintf fmt "Sub (%a,%a)" pp_expr l pp_expr r

let expr_testable = Alcotest.testable pp_expr ( = )

(* ------------------------------
             TOKEN
  ------------------------------- *)
let input_tokens = "-13-(+0123)"

let test_tokens () =
  let open Pratt.Lexer in
  let expected =
    [
      Token.Minus
    ; Token.Int 13
    ; Token.Minus
    ; Token.Lparen
    ; Token.Plus
    ; Token.Int 123
    ; Token.Rparen
    ]
  in
  Alcotest.(check (list token_testable))
    "some tokens" expected (tokenize input_tokens)

(* ------------------------------
             PARSER
  ------------------------------- *)
let test_plus5 () =
  let open Pratt in
  let tokens = Lexer.tokenize "+5" in
  let state = Parser.{ tokens } in
  let _, ast = Parser.parse_expr state in
  let expected = Parser.Int 5 in
  Alcotest.(check expr_testable) "parse +5" expected ast

let test_minus5 () =
  let open Pratt in
  let tokens = Lexer.tokenize "-5" in
  let state = Parser.{ tokens } in
  let _, ast = Parser.parse_expr state in
  let expected = Parser.Neg (Parser.Int 5) in
  Alcotest.(check expr_testable) "parse -5" expected ast

let test_parens () =
  let open Pratt in
  let tokens = Lexer.tokenize "(-2)" in
  let state = Parser.{ tokens } in
  let _, ast = Parser.parse_expr state in
  let expected = Parser.Neg (Parser.Int 2) in
  Alcotest.(check expr_testable) "parse (-2)" expected ast

let test_nested () =
  let open Pratt in
  let tokens = Lexer.tokenize "+(-(-5))" in
  let state = Parser.{ tokens } in
  let _, ast = Parser.parse_expr state in
  let expected = Parser.Neg (Parser.Neg (Parser.Int 5)) in
  Alcotest.(check expr_testable) "parse +(-(-5))" expected ast

(* ------------------------------
             RUN IT
  ------------------------------- *)
let () =
  let open Alcotest in
  run "Pratt testsuite"
    [
      ("tokeniazer", [ test_case input_tokens `Slow test_tokens ])
    ; ( "parser"
      , [
          Alcotest.test_case "parse +5" `Quick test_plus5
        ; Alcotest.test_case "parse -5" `Quick test_minus5
        ; Alcotest.test_case "parse (-2)" `Quick test_parens
        ; Alcotest.test_case "parse +(-(-5))" `Quick test_nested
        ] )
    ]
