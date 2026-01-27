let pp_token_type fmt tt =
  Format.fprintf fmt "%s" (Monkey.Token.Type.to_string tt)

let token_type = Alcotest.testable pp_token_type ( = )

let pp_statement_type fmt stmt =
  Format.fprintf fmt "%s" (Monkey.Ast.Statement.token_literal stmt)

let statement_type = Alcotest.testable pp_statement_type ( = )

let check_tokens lexer expected =
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
