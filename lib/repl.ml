let rec print_tokens lexer =
  let token, lexer' = Lexer.next_token lexer in
  match token with
  | { ty = Token.Eof; _ } -> ()
  | _ ->
      Printf.printf "%s\n" (Token.string_of_token token);
      print_tokens lexer'

let start () =
  print_endline "Hello, This is the Monkey programming language!";
  print_endline "Feel free to type in commands, Ctrl-D to quit.";

  let rec loop () =
    print_string ">> ";
    try
      read_line () |> Lexer.create |> print_tokens;
      (* Here we flush all because in debug mode output is done
         on stderr. *)
      flush_all ();
      loop ()
    with End_of_file -> print_endline "Bye"
  in
  loop ()
