module A = Ast
module L = Lexer
module T = Token

type t = { tokens : T.t Seq.t; cur_token : T.t; peek_token : T.t }

let new_parser (l : L.t) : t =
  let toks = L.tokens l in
  let cur_token, toks = L.next_token toks in
  let peek_token, toks = L.next_token toks in
  { tokens = toks; cur_token; peek_token }

let next_token (parse : t) : t =
  let parse = { parse with cur_token = parse.peek_token } in
  let peek_token, tokens = L.next_token parse.tokens in
  { parse with peek_token; tokens }

(* Entry point to parse program *)
let rec parse_program (parse : t) : A.Program.t = parse_stmts parse []

(* STATEMENTS *)
and parse_stmts (parse : t) (stmts : A.Program.t) : A.Program.t =
  match parse.cur_token with
  | T.EOF -> List.rev stmts
  | T.Let -> parse_let_statement parse stmts
  | t ->
      Format.printf "Got %a\n" T.pp t;
      failwith "TODO: Currently only EOF and Let are supported"

(* LET STATEMENT
   LET <IDENT> = <EXPR> ;
*)
and parse_let_statement (parse : t) (stmts : A.Statement.t list) :
    A.Statement.t list =
  (* First we are expecting an Identifier *)
  match parse.peek_token with
  | T.Ident _ -> parse_let_statement_assign (next_token parse) stmts
  | t ->
      Format.printf "Got %a\n" T.pp t;
      failwith "ERROR: Expected identifier"

and parse_let_statement_assign (parse : t) (stmts : A.Statement.t list) :
    A.Statement.t list =
  (* Then we are expecting an assignement *)
  match parse.peek_token with
  | T.Assign -> parse_let_statement_expr (next_token parse) stmts
  | _ -> failwith "ERROR: Expected assign"

and parse_let_statement_expr (parse : t) (stmts : A.Statement.t list) :
    A.Statement.t list =
  (* And finally we expect an expression.
     As we don't parse expresion yet we just skip it until getting a semicolon *)
  let rec loop (p : t) : t =
    match p.cur_token with
    | T.Semicolon -> next_token p
    | _ -> loop (next_token p)
  in
  let parse = loop parse in
  parse_stmts parse stmts
