module L = Monkey.Lexer
module T = Monkey.Token

(** To be able to test token list we need to wrap T.t in a module that
   provides a pretty printer function and an equality checking function. *)
let token_list =
  let token_list_equal xs ys =
    List.length xs = List.length ys && List.for_all2 ( = ) xs ys
  in
  let module M = struct
    type t = T.t list

    let pp fmt xs =
      let pp_sep fmt () = Format.fprintf fmt ", " in
      Format.fprintf fmt "[%a]" (Format.pp_print_list ~pp_sep T.pp) xs

    let equal = token_list_equal
  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let get_all_tokens_helper lexer =
  L.tokens lexer |> Seq.take_while (fun t -> t != T.EOF) |> List.of_seq

(**********************************************************************
 ** testing read character                                            *
 **********************************************************************)
let test_abc_new () =
  let lexer = L.new_lexer "abc" in
  Alcotest.(check int) "read_position" 1 lexer.read_position;
  Alcotest.(check char) "ch" 'a' lexer.ch

let test_abc_read_once () =
  let lexer = L.(new_lexer "abc" |> read_char) in
  Alcotest.(check int) "read_position" 2 lexer.read_position;
  Alcotest.(check char) "ch" 'b' lexer.ch

let test_abc_read_twice () =
  let lexer = L.(new_lexer "abc" |> read_char |> read_char) in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" 'c' lexer.ch

let test_abc_read_thrice () =
  let lexer = L.(new_lexer "abc" |> read_char |> read_char |> read_char) in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" '\000' lexer.ch

let test_abc_read_more () =
  let lexer =
    L.(new_lexer "abc" |> read_char |> read_char |> read_char |> read_char)
  in
  Alcotest.(check int) "read_position" 3 lexer.read_position;
  Alcotest.(check char) "ch" '\000' lexer.ch

(**********************************************************************
 ** testing token                                                     *
 **********************************************************************)
let test_token_equal () =
  let expected = [ T.EQ ] in
  let lexer = L.new_lexer "==" in
  let tokens = get_all_tokens_helper lexer in
  Alcotest.(check token_list) "same tokens" expected tokens

let test_token_not_equal () =
  let expected = [ T.NotEQ ] in
  let lexer = L.new_lexer "!=" in
  let tokens = get_all_tokens_helper lexer in
  Alcotest.(check token_list) "same tokens" expected tokens

let test_get_first_token () =
  let expected = [ T.Let ] in
  let lexer = L.new_lexer "let a = 10" in
  let toks = L.tokens lexer in
  let first_token, _toks = L.next_token toks in
  Alcotest.(check token_list) "same tokens" expected [ first_token ]

let test_get_first_two_tokens () =
  let expected = [ T.Let; T.Ident "a" ] in
  let lexer = L.new_lexer "let a = 10" in
  let toks = L.tokens lexer in
  let first_token, toks = L.next_token toks in
  let second_token, _toks = L.next_token toks in
  Alcotest.(check token_list)
    "same tokens" expected
    [ first_token; second_token ]

let test_different_tokens () =
  let expected =
    [
      T.Assign;
      T.Semicolon;
      T.LParen;
      T.RParen;
      T.Comma;
      T.Plus;
      T.LBrace;
      T.RBrace;
    ]
  in
  let lexer = L.new_lexer " = ; ( ) , + { } " in
  let tokens = get_all_tokens_helper lexer in
  Alcotest.(check token_list) "same tokens" expected tokens

let test_let_statement () =
  let expected = [ T.Let; T.Ident "un"; T.Assign; T.Int 1; T.Semicolon ] in
  let lexer = L.new_lexer "let un = 1;" in
  let tokens = get_all_tokens_helper lexer in
  Alcotest.(check token_list) "same tokens" expected tokens
