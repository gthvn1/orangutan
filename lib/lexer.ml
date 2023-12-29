type t = { input : string; position : int; read_position : int; ch : char }

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

let read_char (lexer : t) : t =
  if lexer.read_position >= String.length lexer.input then
    { lexer with ch = '\000' }
  else
    {
      input = lexer.input;
      position = lexer.read_position;
      read_position = lexer.read_position + 1;
      ch = String.get lexer.input lexer.read_position;
    }

let%test "read_char_0" =
  let lexer = new_lexer "abc" in
  lexer.ch = 'a' && lexer.position = 0 && lexer.read_position = 1

let%test "read_char_1" =
  let lexer = new_lexer "abc" in
  let lexer = read_char lexer in
  lexer.ch = 'b' && lexer.position = 1 && lexer.read_position = 2

let%test "read_char_2" =
  let lexer = new_lexer "abc" in
  let lexer = read_char lexer in
  let lexer = read_char lexer in
  lexer.ch = 'c' && lexer.position = 2 && lexer.read_position = 3

let%test "read_char_3" =
  let lexer = new_lexer "abc" in
  let lexer = read_char lexer in
  let lexer = read_char lexer in
  let lexer = read_char lexer in
  lexer.ch = '\000' && lexer.position = 2 && lexer.read_position = 3

let%test "read_char_4" =
  let lexer = new_lexer "abc" in
  let lexer = read_char lexer in
  let lexer = read_char lexer in
  let lexer = read_char lexer in
  let lexer = read_char lexer in
  lexer.ch = '\000' && lexer.position = 2 && lexer.read_position = 3
