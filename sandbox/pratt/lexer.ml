(*
   We want to parse: +5 - (-2)
   We support integer, addition, soustraction and multiplication
 *)

module Token = struct
  type t = Plus | Minus | Lparen | Rparen | Int of int

  let to_string = function
    | Plus -> "+"
    | Minus -> "-"
    | Lparen -> "("
    | Rparen -> ")"
    | Int x -> string_of_int x
end

let is_digit c = '0' <= c && c <= '9'
let char_to_int c = Char.code c - Char.code '0'

let tokenize (input : string) : Token.t list =
  let digits_to_int (digits : char list) : int =
    digits |> List.to_seq |> String.of_seq |> int_of_string
  in
  let flush_number acc digits =
    match digits with
    | [] -> acc
    | _ ->
        let n = digits |> List.rev |> digits_to_int in
        Token.Int n :: acc
  in
  let rec loop (num : char list) (acc : Token.t list) (l : char list) =
    match l with
    | [] -> flush_number acc num |> List.rev
    | c :: cs ->
        if is_digit c then loop (c :: num) acc cs
        else
          let token =
            match c with
            | '+' -> Token.Plus
            | '-' -> Token.Minus
            | '(' -> Token.Lparen
            | ')' -> Token.Rparen
            | _ -> failwith (Printf.sprintf "unknown %c" c)
          in
          let acc = flush_number acc num in
          loop [] (token :: acc) cs
  in
  let chars = String.to_seq input |> List.of_seq in
  loop [] [] chars
