(** Global variable to enable/disable debug info *)
let debug = ref true

type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }
(** [debug_lexer label lexer] prints the state of the lexer prepending a
    [label]. *)

type program = Ast.statement list
(** [program] is the root of every AST. It is simply a list of statements,
    representing the Monkey program. *)

let debug_parser (label : string) (parser : t) : unit =
  if !debug then
    Printf.eprintf "%s  current: %s  peek: %s\n%!" label
      (Token.string_of_token parser.cur_token)
      (Token.string_of_token parser.peek_token)

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

(** [parse_program parser] is the entry point for parsing tokens. It expects a
    valid [parser] and will return a list of statement. *)
let rec parse_program (parser : t) : program =
  let rec loop prog p =
    debug_parser "parse program loop" p;
    match p.cur_token.ty with
    | Token.Eof -> List.rev prog
    | Token.Let ->
        let stmt, p' = parse_let_statement p in
        loop (stmt :: prog) (next_token p')
    | _ -> failwith "TODO: parse other statement"
  in
  loop [] parser

and parse_let_statement (parser : t) : Ast.statement * t =
  debug_parser "[let] begin" parser;

  let stmt_token = parser.cur_token in

  let parser = next_token parser in
  debug_parser "[let] let identifier" parser;
  (* We are expecting an Identifier after the LET token *)
  if parser.cur_token.ty <> Token.Ident then
    failwith "Parser error: we are expecting an identifier after LET";
  let name : Ast.identifier =
    { token = parser.cur_token; value = parser.cur_token.literal }
  in

  (* TODO: parse expression. Until we implement it we advance until finding SEMICOLON *)
  let e : Ast.expression = () in
  let rec loop p =
    if p.cur_token.ty <> Token.Semicolon then loop (next_token p) else p
  in
  let parser = loop parser in
  debug_parser "[let] skip expression" parser;
  (Let { token = stmt_token; name; value = e }, parser)
