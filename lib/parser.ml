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
  match parse.peek_token with
  | T.Ident _ -> parse_let_statement_ident (next_token parse) stmts
  | t ->
      Format.printf "Got %a\n" T.pp t;
      failwith "ERROR: Expected identifier"

and parse_let_statement_ident (parse : t) (stmts : A.Statement.t list) :
    A.Statement.t list =
  match parse.peek_token with
  | T.Assign -> parse_let_statement_assign (next_token parse) stmts
  | _ -> failwith "ERROR: Expected assign"

and parse_let_statement_assign (parse : t) (stmts : A.Statement.t list) :
    A.Statement.t list =
  match parse.peek_token with
  | T.Ident _ -> parse_let_statement_assign_ident (next_token parse) stmts
  | t ->
      Format.printf "Got %a\n" T.pp t;
      failwith "ERROR: Expected Expression"

and parse_let_statement_assign_ident (parse : t) (stmts : A.Statement.t list) :
    A.Statement.t list =
  match parse.peek_token with
  | T.Semicolon -> parse_stmts (next_token parse |> next_token) stmts
  (* We call next_token twice because parse_stmts checks current token so we need
     to advance twice to have EOF under current token. Will be modified in future
     probably.*)
  | _ -> failwith "ERROR: Expected semicolon"
