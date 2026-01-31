type expr =
  | Int of int
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Neg of expr

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

(** [infix_binding_power token] returns a tuple that is the left and right power
    of a given token. The higher number is, the higher the precedence is. To
    preserve left associativity lbp < rbp. *)
let infix_binding_power = function
  | Lexer.Token.Plus | Lexer.Token.Minus -> (10, 11)
  | Lexer.Token.Mult | Lexer.Token.Div -> (20, 21)
  | _ -> failwith "not an infix operator"

(* --------------------------------------------------------------------
                            PARSER
   -------------------------------------------------------------------- *)

let rec parse_expr (s : state) (bp : int) : state * expr =
  (* 1. parse the prefix part of the expression *)
  let s, left = parse_prefix s in

  (* 2. Pratt infix loop *)
  let rec loop s left =
    let open Lexer in
    match peek s with
    | Some ((Token.Plus | Token.Minus | Token.Mult | Token.Div) as op) ->
        let lbp, rbp = infix_binding_power op in
        if lbp < bp then (s, left)
        else
          let s = consume s in
          let s, right = parse_expr s rbp in
          let left =
            match op with
            | Token.Plus -> Add (left, right)
            | Token.Minus -> Sub (left, right)
            | Token.Mult -> Mul (left, right)
            | Token.Div -> Div (left, right)
            | _ -> failwith "not reachable"
          in
          loop s left
    | _ -> (s, left)
  in
  loop s left

(*
   What can start an expression ?
   We can only have:
    - an integer   : 5 + ..
    - a prefix +   : +8 * ...
    - a prefix -   : -2 + ...
    - a left parenthesis : ( ...
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
      (* unary minus binds tighter than infix operator *)
      let s, e = parse_expr s 100 in
      (s, Neg e)
  | Some Lexer.Token.Lparen -> (
      let s = consume s in
      (* parenthesis starts a new expression so we call parse_expr with 0 binding power *)
      let s, e = parse_expr s 0 in
      (* Here we are expecting: '(' expr ')' *)
      match peek s with
      | Some Lexer.Token.Rparen ->
          let s = consume s in
          (s, e)
      | _ -> failwith "Right parenthesis is expected to close expression")
  | Some t -> failwith ("Unexpected token: " ^ Lexer.Token.to_string t)
  | None -> failwith "Unexpected end of input"

(** [parse tokens] returns the AST that corresponds to the list of [tokens]. *)
let parse tokens : expr =
  let s = { tokens } in
  let s, expr = parse_expr s 0 in
  match s.tokens with
  | [] -> expr
  | _ -> failwith "unexpected tokens at the end"
