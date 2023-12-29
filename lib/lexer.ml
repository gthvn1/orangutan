type t = {
  input : string;
  mutable position : int;
  mutable read_position : int;
  mutable ch : char;
}

let new_lexer (str : string) : t =
  {
    input = str;
    position = 0;
    (* current position *)
    read_position = 1;
    (* after current char, the reading position *)
    ch = String.get str 0;
    (* current char *)
  }

let read_char (lexer : t) : unit =
  if lexer.read_position >= String.length lexer.input then lexer.ch <- '\000'
  else (
    lexer.position <- lexer.read_position;
    lexer.read_position <- lexer.read_position + 1;
    lexer.ch <- String.get lexer.input lexer.position)

let next_token (_lexer : t) : Token.t = failwith "TODO"

(**********************************************************************)
(* TESTS                                                              *)
(**********************************************************************)
let%test "read_char_0" =
  let lexer = new_lexer "abc" in
  lexer.ch = 'a' && lexer.position = 0 && lexer.read_position = 1

let%test "read_char_1" =
  let lexer = new_lexer "abc" in
  read_char lexer;
  lexer.ch = 'b' && lexer.position = 1 && lexer.read_position = 2

let%test "read_char_2" =
  let lexer = new_lexer "abc" in
  read_char lexer;
  read_char lexer;
  lexer.ch = 'c' && lexer.position = 2 && lexer.read_position = 3

let%test "read_char_3" =
  let lexer = new_lexer "abc" in
  read_char lexer;
  read_char lexer;
  read_char lexer;
  lexer.ch = '\000' && lexer.position = 2 && lexer.read_position = 3

let%test "read_char_4" =
  let lexer = new_lexer "abc" in
  read_char lexer;
  read_char lexer;
  read_char lexer;
  read_char lexer;
  lexer.ch = '\000' && lexer.position = 2 && lexer.read_position = 3
