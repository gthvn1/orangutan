(**********************************************************************)
(* TESTS                                                              *)
(**********************************************************************)
module Lexer = Monkey.Lexer
module Token = Monkey.Token

(** To be able to test token list we need to wrap Token.t in a module that
   provides a pretty printer function and an equality checking function. *)
let token_list =
  let token_list_equal xs ys =
    List.length xs = List.length ys && List.for_all2 ( = ) xs ys
  in
  let module M = struct
    type t = Token.t list

    let pp fmt xs =
      let pp_sep fmt () = Format.fprintf fmt ", " in
      Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep Token.pp) xs

    let equal = token_list_equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let test_abc_new () =
  let lexer = Lexer.new_lexer "abc" in
  Alcotest.(check int) "read_position" 1 lexer.read_position;
  Alcotest.(check char) "ch" 'a' lexer.ch

let test_abc_read_once () =
  let lexer = Lexer.(new_lexer "abc" |> read_char) in
  Alcotest.(check int) "read_position" 2 lexer.read_position;
  Alcotest.(check char) "ch" 'b' lexer.ch

let test_abc_read_twice () =
  let lexer = Lexer.(new_lexer "abc" |> read_char |> read_char) in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" 'c' lexer.ch

let test_abc_read_thrice () =
  let lexer = Lexer.(new_lexer "abc" |> read_char |> read_char |> read_char) in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" '\000' lexer.ch

let test_abc_read_more () =
  let lexer =
    Lexer.(new_lexer "abc" |> read_char |> read_char |> read_char |> read_char)
  in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" '\000' lexer.ch

let test_two_assign () =
  let expected = [ Token.Assign; Token.Assign ] in
  let lexer = Lexer.new_lexer "==" in
  let tokens =
    Lexer.next_token lexer
    |> Seq.take_while (fun t -> t != Token.EOF)
    |> List.of_seq
  in
  Alcotest.(check int) "same length" (List.length expected) (List.length tokens);
  Alcotest.(check token_list) "same tokens" expected tokens

let test_different_tokens () =
  let expected =
    [
      Token.Assign;
      Token.Semicolon;
      Token.LParen;
      Token.RParen;
      Token.Comma;
      Token.Plus;
      Token.LBrace;
      Token.RBrace;
    ]
  in
  let lexer = Lexer.new_lexer "=;(),+{}" in
  let tokens =
    Lexer.next_token lexer
    |> Seq.take_while (fun t -> t != Token.EOF)
    |> List.of_seq
  in
  Alcotest.(check int) "same length" (List.length expected) (List.length tokens);
  Alcotest.(check token_list) "same tokens" expected tokens

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
      ( "token",
        [
          test_case "two assigns" `Quick test_two_assign;
          test_case "different tokens" `Quick test_different_tokens;
        ] );
    ]
