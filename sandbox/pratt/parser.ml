type expr = Int of int | Add of expr * expr | Sub of expr * expr | Neg of expr
type state = { tokens : Lexer.Token.t list }

(** [peek s] returns the next token. If there is no more tokens it returns None.
    State [s] is not modified. It looks without committing. *)
let peek (s : state) : Lexer.Token.t option =
  match s.tokens with
  | [] -> None
  | t :: _ -> Some t

(** [consume s] consumes the token in the head of the list and returns the new
    state. It raises an exception if there is no token to consume. *)
let consume (s : state) : state =
  match s.tokens with
  | [] -> failwith "no more tokens to consume"
  | _ :: xs -> { tokens = xs }

let rec parse_expr (s : state) : state * expr =
  (* TODO: really parse an expression *)
  parse_prefix s

(* What can start an expression ?
   We can only have:
    - an integer
    - a prefix +
    - a prefix -
    - a left parenthesis
 *)
and parse_prefix (s : state) : state * expr =
  match peek s with
  | Some (Lexer.Token.Int n) ->
      let s = consume s in
      (s, Int n)
  | Some Lexer.Token.Plus ->
      (* Plus as a prefix is just ignored *)
      consume s |> parse_prefix
  | Some Lexer.Token.Minus ->
      let s = consume s in
      let s, e = parse_prefix s in
      (s, Neg e)
  | Some Lexer.Token.Lparen -> (
      let s = consume s in
      let s, e = parse_expr s in
      (* Here we are expecting: '(' expr ')' *)
      match peek s with
      | Some Lexer.Token.Rparen -> (s, e)
      | _ -> failwith "Right parenthesis is expected to close expression")
  | Some t -> failwith ("Unexpected token: " ^ Lexer.Token.to_string t)
  | None -> failwith "Unexpected end of input"
