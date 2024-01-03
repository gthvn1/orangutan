module L = Monkey.Lexer
module P = Monkey.Parser
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

let test_parser_new () =
  let p = "let x = 5;" |> L.new_lexer |> P.new_parser in
  Alcotest.(check token_list)
    "test create of parser"
    [ fst p; P.peek_token p ]
    [ T.Let; T.Ident "x" ]

let test_parse_let_assign_int () =
  let prog = "let x = 12;" |> L.new_lexer |> P.new_parser |> P.parse_program in
  Alcotest.(check int) "check 1 stmts" (List.length prog.stmts) 1;
  Alcotest.(check int) "check no errors" (List.length prog.errors) 0

let test_parse_let_assign_ident () =
  let prog = "let x = y;" |> L.new_lexer |> P.new_parser |> P.parse_program in
  Alcotest.(check int) "check 1 stmts" (List.length prog.stmts) 1;
  Alcotest.(check int) "check no errors" (List.length prog.errors) 0

let test_parse_three_let () =
  let prog =
    "let x = 5; let y = 10; let foobar = 838383;" |> L.new_lexer |> P.new_parser
    |> P.parse_program
  in
  Alcotest.(check int) "check 3 stmts" (List.length prog.stmts) 3;
  Alcotest.(check int) "check no errors" (List.length prog.errors) 0

let test_parse_return () =
  let prog = "return true;" |> L.new_lexer |> P.new_parser |> P.parse_program in
  Alcotest.(check int) "check 1 stmts" (List.length prog.stmts) 1;
  Alcotest.(check int) "check no errors" (List.length prog.errors) 0
