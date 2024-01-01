let () =
  let open Alcotest in
  run "MonkeyTest"
    [
      ( "Lexer: abc_lexer",
        [
          test_case "abc init" `Quick Test_lexer.test_abc_new;
          test_case "read one character" `Quick Test_lexer.test_abc_read_once;
          test_case "read two characters" `Quick Test_lexer.test_abc_read_twice;
          test_case "read three characters" `Quick
            Test_lexer.test_abc_read_thrice;
          test_case "read more than three characters" `Quick
            Test_lexer.test_abc_read_more;
        ] );
      ( "Lexer: token",
        [
          test_case "token equal" `Quick Test_lexer.test_token_equal;
          test_case "token not_equal" `Quick Test_lexer.test_token_not_equal;
          test_case "get first token" `Quick Test_lexer.test_get_first_token;
          test_case "different tokens" `Quick Test_lexer.test_different_tokens;
          test_case "let un = 1;" `Quick Test_lexer.test_let_statement;
        ] );
      ( "Parser: generic",
        [
          test_case "create parser" `Quick Test_parser.test_parser_new;
          test_case "parsing 'let a = 12;'" `Quick
            Test_parser.test_parse_let_assign_int;
          test_case "parsing 'let x = y;'" `Quick
            Test_parser.test_parse_let_assign_ident;
        ] );
    ]
