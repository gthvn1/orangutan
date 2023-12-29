type t = { input : string; read_position : int; ch : char }

let new_lexer (str : string) : t =
  {
    input = str;
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
      lexer with
      ch = String.get lexer.input lexer.read_position;
      read_position = lexer.read_position + 1;
    }

let next_token (lexer : t) : Token.t =
  let _ = lexer.ch in
  (* just here to use lexer and ch field *)
  failwith "TODO"
