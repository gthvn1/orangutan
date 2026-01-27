(** Global variable to enable/disable debug info *)
let debug = ref true

module Stmt = Ast.Statement

type t = {
    lexer : Lexer.t
  ; cur_token : Token.t
  ; peek_token : Token.t
  ; errors : string list
}
(** [debug_lexer label lexer] prints the state of the lexer prepending a
    [label]. *)

type program = Stmt.t list
(** [program] is the root of every AST. It is simply a list of statements,
    representing the Monkey program. *)

let debug_parser (label : string) (parser : t) : unit =
  if !debug then
    Printf.eprintf "%s current:%s peek:%s\n%!" label
      (Token.string_of_token parser.cur_token)
      (Token.string_of_token parser.peek_token)

let ( let* ) = Result.bind

(** [next_token parser] returns a new parser where current token and peek token
    have been updated. *)
let next_token (parser : t) : t =
  let next_token, next_lexer = Lexer.next_token parser.lexer in
  {
    parser with
    lexer = next_lexer
  ; cur_token = parser.peek_token
  ; peek_token = next_token
  }

(** [create lexer] returns a parser initialized where current token and peek
    token are both set. *)
let create (lexer : Lexer.t) : t =
  let first_token, lexer = Lexer.next_token lexer in
  let second_token, lexer = Lexer.next_token lexer in
  { lexer; cur_token = first_token; peek_token = second_token; errors = [] }

(** [expect_peek parser token] returns the updated parser and true if [token]
    matches the peek token. Otherwise parser is not updated and false is
    returned. *)
let expect_peek (parser : t) ~(token_type : Token.Type.t) : (t, string) result =
  if parser.peek_token.ty = token_type then Ok (next_token parser)
  else
    Error
      (Printf.sprintf "expected next token to be %s, got %s instead"
         (Token.Type.to_string token_type)
         (Token.Type.to_string parser.peek_token.ty))

(** [cur_token_type_is parser token] returns true if the current token of
    [parser] matches [token]. It returns false otherwise. *)
let cur_token_type_is (parser : t) ~(token : Token.Type.t) : bool =
  parser.cur_token.ty = token

(** [peek_token_type_is parser token] returns true if the peek token of [parser]
    matches [token]. It returns false otherwise. *)
let peek_token_type_is (parser : t) ~(token : Token.Type.t) : bool =
  parser.peek_token.ty = token

(** [parse_program parser] is the entry point for parsing tokens. It expects a
    valid [parser] and will return a list of statement. *)
let rec parse_program (parser : t) : program * string list =
  let open Token.Type in
  let rec loop prog p =
    match p.cur_token.ty with
    | Eof -> (List.rev prog, List.rev p.errors)
    | Let -> (
        match parse_let_statement p with
        | Ok (stmt, p') -> loop (stmt :: prog) (next_token p')
        | Error e -> loop prog (next_token { p with errors = e :: p.errors }))
    | Return -> (
        match parse_return_statement p with
        | Ok (stmt, p') -> loop (stmt :: prog) (next_token p')
        | Error e -> loop prog (next_token { p with errors = e :: p.errors }))
    | _ -> loop prog (next_token p)
  in
  loop [] parser

and parse_let_statement (parser : t) : (Stmt.t * t, string) result =
  debug_parser "[let] begin" parser;
  let stmt_token = parser.cur_token in

  (* After the LET token we are expecting an Identifier *)
  let* parser = expect_peek parser ~token_type:Token.Type.Ident in
  debug_parser "[let] found identifier" parser;
  let name : Ast.Identifier.t =
    { token = parser.cur_token; value = parser.cur_token.literal }
  in

  (* After the identifier we are expecting an assignement *)
  let* parser = expect_peek parser ~token_type:Token.Type.Assign in
  debug_parser "[let] found assign" parser;

  (* After Assignement we are expecting the expression.
     TODO: parse expression. Until we implement it, advance until SEMICOLON *)
  let rec skip_expression p =
    if cur_token_type_is p ~token:Token.Type.Semicolon then p
    else skip_expression (next_token p)
  in
  let parser = skip_expression parser in
  debug_parser "[let] skip expression" parser;

  (* We can now return the statement and the new parser state *)
  Ok (Stmt.Let { token = stmt_token; name; value = () }, parser)

and parse_return_statement (parser : t) : (Stmt.t * t, string) result =
  debug_parser "[return] begin" parser;
  let stmt_token = parser.cur_token in

  let parser = next_token parser in
  (* TODO: we are skipping expression until we encounter semi colon *)
  let rec skip_expression p =
    if cur_token_type_is p ~token:Token.Type.Semicolon then p
    else skip_expression (next_token p)
  in
  let parser = skip_expression parser in
  debug_parser "[return] skip expression" parser;

  Ok (Stmt.Return { token = stmt_token; value = () }, parser)
