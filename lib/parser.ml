module A = Ast
module L = Lexer
module T = Token

(* a tuple where first element is the current token and the second element is
   the sequence of remaining tokens *)
type t = T.t * T.t Seq.t

let new_parser (l : L.t) : t =
  let toks = L.tokens l in
  L.next_token toks

let next_token (parse : t) : t = L.next_token @@ snd parse
let peek_token (parse : t) : T.t = parse |> snd |> L.tokens_hd

(*****************************************************************************)
(* LET STATEMENT  LET <IDENT> = <EXPR> ;                                     *)
(*****************************************************************************)
let parse_let_statement_expr (parse : t) (ident : A.identifier) :
    (t * A.Statement.t, string) result =
  (* And finally we expect an expression. As we don't parse expresion yet we
     just skip it until getting a semicolon *)
  let rec loop (p : t) : t =
    match fst p with T.Semicolon -> next_token p | _ -> loop (next_token p)
  in
  let parse = loop parse in
  Ok
    ( parse,
      A.Statement.Let { token = T.Let; name = ident; value = A.Expression.ToDo }
    )

let parse_let_statement_assign (parse : t) (ident : A.identifier) :
    (t * A.Statement.t, string) result =
  (* Then we are expecting an assignement *)
  match peek_token parse with
  | T.Assign -> parse_let_statement_expr (next_token parse) ident
  | t -> Error ("Expected assign, got " ^ T.to_string t)

let parse_let_statement (parse : t) : (t * A.Statement.t, string) result =
  (* First we are expecting an Identifier *)
  match peek_token parse with
  | T.Ident value as token ->
      parse_let_statement_assign (next_token parse) { token; value }
  | t -> Error ("Expected identifier, got " ^ T.to_string t)

(*****************************************************************************)
(* Entry point to parse program                                              *)
(*****************************************************************************)
let parse_program (parse : t) : A.Program.t =
  let prog : A.Program.t = { stmts = []; errors = [] } in
  let rec parse_stmts (parse : t) (prog : A.Program.t) : A.Program.t =
    if fst parse = T.EOF then
      { stmts = List.rev prog.stmts; errors = List.rev prog.errors }
    else
      let parsed_stmt =
        match fst parse with
        | T.Let -> parse_let_statement parse
        | t -> Error ("Parsing " ^ T.to_string t ^ " is not yet supported.")
      in
      match parsed_stmt with
      | Error e ->
          parse_stmts (next_token parse) { prog with errors = e :: prog.errors }
      | Ok (p, s) ->
          parse_stmts p { stmts = s :: prog.stmts; errors = prog.errors }
  in
  parse_stmts parse prog
