(** Global variable to enable/disable debug info *)
let debug = ref true

type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }
(** [debug_lexer label lexer] prints the state of the lexer prepending a
    [label]. *)

type program = Ast.Statement.t list
(** [program] is the root of every AST. It is simply a list of statements,
    representing the Monkey program. *)

type parse_error = WrongPeekToken | UnkownStatement

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
  { lexer = next_lexer; cur_token = parser.peek_token; peek_token = next_token }

(** [create lexer] returns a parser initialized where current token and peek
    token are both set. *)
let create (lexer : Lexer.t) : t =
  let first_token, lexer = Lexer.next_token lexer in
  let second_token, lexer = Lexer.next_token lexer in
  { lexer; cur_token = first_token; peek_token = second_token }

(** [expect_peek parser token] returns the updated parser and true if [token]
    matches the peek token. Otherwise parser is not updated and false is
    returned. *)
let expect_peek (parser : t) ~(token : Token.token_type) :
    (t, parse_error) result =
  if parser.peek_token.ty = token then Ok (next_token parser)
  else Error WrongPeekToken

(** [cur_token_is parser token] returns true if the current token of [parser]
    matches [token]. It returns false otherwise. *)
let cur_token_is (parser : t) ~(token : Token.token_type) : bool =
  parser.cur_token.ty = token

(** [peek_token_is parser token] returns true if the peek token of [parser]
    matches [token]. It returns false otherwise. *)
let peek_token_is (parser : t) ~(token : Token.token_type) : bool =
  parser.peek_token.ty = token

(** [parse_program parser] is the entry point for parsing tokens. It expects a
    valid [parser] and will return a list of statement. *)
let rec parse_program (parser : t) : (program, parse_error) result =
  let rec loop prog p =
    match p.cur_token.ty with
    | Token.Eof -> Ok (List.rev prog)
    | Token.Let ->
        let* stmt, p' = parse_let_statement p in
        loop (stmt :: prog) (next_token p')
    | _ -> Error UnkownStatement
  in
  loop [] parser

and parse_let_statement (parser : t) : (Ast.Statement.t * t, parse_error) result
    =
  debug_parser "[let] begin" parser;
  let stmt_token = parser.cur_token in

  (* After the LET token we are expecting an Identifier *)
  let* parser = expect_peek parser ~token:Token.Ident in
  debug_parser "[let] found identifier" parser;
  let name : Ast.Identifier.t =
    { token = parser.cur_token; value = parser.cur_token.literal }
  in

  (* After the identifier we are expecting an assignement *)
  let* parser = expect_peek parser ~token:Token.Assign in
  debug_parser "[let] found assign" parser;

  (* After Assignement we are expecting the expression.
     TODO: parse expression. Until we implement it we advance until finding SEMICOLON *)
  let rec skip_expression p =
    if cur_token_is p ~token:Token.Semicolon then p
    else skip_expression (next_token p)
  in
  let parser = skip_expression parser in
  debug_parser "[let] skip expression" parser;

  (* We can now return the statement and the new parser state *)
  Ok (Ast.Statement.Let { token = stmt_token; name; value = () }, parser)
