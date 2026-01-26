let ping () = "pong"

type t = { lexer : Lexer.t; cur_token : Token.t; peek_token : Token.t }

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

let parse_program (_parser : t) : Ast.program = failwith "todo"
