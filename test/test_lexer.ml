(**********************************************************************)
(* TESTS                                                              *)
(**********************************************************************)
let test_abc_new () =
  let lexer = Monkey.Lexer.new_lexer "abc" in
  Alcotest.(check int) "read_position" 1 lexer.read_position;
  Alcotest.(check char) "ch" 'a' lexer.ch

let test_abc_read_once () =
  let lexer = Monkey.Lexer.(new_lexer "abc" |> read_char) in
  Alcotest.(check int) "read_position" 2 lexer.read_position;
  Alcotest.(check char) "ch" 'b' lexer.ch

let test_abc_read_twice () =
  let lexer = Monkey.Lexer.(new_lexer "abc" |> read_char |> read_char) in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" 'c' lexer.ch

let test_abc_read_thrice () =
  let lexer =
    Monkey.Lexer.(new_lexer "abc" |> read_char |> read_char |> read_char)
  in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" '\000' lexer.ch

let test_abc_read_more () =
  let lexer =
    Monkey.Lexer.(
      new_lexer "abc" |> read_char |> read_char |> read_char |> read_char)
  in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" '\000' lexer.ch

let () =
  let open Alcotest in
  run "Lexer"
    [
      ( "abc_lexer",
        [
          test_case "abc init" `Quick test_abc_new;
          test_case "read one character" `Quick test_abc_read_once;
          test_case "read two characters" `Quick test_abc_read_twice;
          test_case "read three characters" `Quick test_abc_read_thrice;
          test_case "read more than three characters" `Quick test_abc_read_more;
        ] );
    ]
