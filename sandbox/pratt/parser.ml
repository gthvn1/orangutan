type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Neg of expr
  | Pos of expr

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

(*
  We want to parse: +5 - (-2)
  => We want to reach: Sub (Pos(Int 5), Neg(Int 2))
*)
