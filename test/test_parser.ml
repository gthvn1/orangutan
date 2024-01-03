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
  let input = "let x = 5;" in
  let lexer = L.new_lexer input in
  let parse = P.new_parser lexer in
  Alcotest.(check token_list)
    "test create of parser"
    [ fst parse; P.peek_token parse ]
    [ T.Let; T.Ident "x" ]

let test_parse_let_assign_int () =
  let input = "let x = 12;" in
  let lexer = L.new_lexer input in
  let parse = P.new_parser lexer in
  let program = P.parse_program parse in
  Alcotest.(check int) "check 1 stmts" (List.length program.stmts) 1;
  Alcotest.(check int) "check no errors" (List.length program.errors) 0

let test_parse_let_assign_ident () =
  let input = "let x = y;" in
  let lexer = L.new_lexer input in
  let parse = P.new_parser lexer in
  let program = P.parse_program parse in
  Alcotest.(check int) "check 1 stmts" (List.length program.stmts) 1;
  Alcotest.(check int) "check no errors" (List.length program.errors) 0

let test_parse_three_let () =
  let input = "let x = 5; let y = 10; let foobar = 838383;" in
  let lexer = L.new_lexer input in
  let parse = P.new_parser lexer in
  let program = P.parse_program parse in
  Alcotest.(check int) "check 3 stmts" (List.length program.stmts) 3;
  Alcotest.(check int) "check no errors" (List.length program.errors) 0

let test_parse_return () =
  let input = "return true;" in
  let lexer = L.new_lexer input in
  let parse = P.new_parser lexer in
  let program = P.parse_program parse in
  Alcotest.(check int) "check 1 stmts" (List.length program.stmts) 1;
  Alcotest.(check int) "check no errors" (List.length program.errors) 0
