type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }

type program = Ast.statement list
(** [program] is the root of every AST. It is simply a list of statements,
    representing the Monkey program. *)

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

let parse_let_statement (_parser : t) : Ast.statement =
  failwith "TODO: implement parse let statement"

let parse_program (parser : t) : program =
  let rec loop prog p =
    match parser.cur_token.ty with
    | Token.Eof -> List.rev prog
    | Token.Let ->
        let stmt = parse_let_statement p in
        loop (stmt :: prog) (next_token p)
    | _ -> failwith "TODO: parse other statement"
  in
  loop [] parser
