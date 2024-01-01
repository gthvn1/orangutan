module L = Monkey.Lexer
module T = Monkey.Token
module F = Format

let rec repl () =
  print_string ">> ";
  let line = read_line () in
  if String.compare "q;" line <> 0 then (
    let lexer = L.new_lexer line in
    L.tokens lexer
    |> Seq.take_while (fun t -> t <> T.EOF)
    |> Seq.iter (fun t -> F.printf "%a " T.pp t);
    F.print_newline ();
    F.print_flush ();
    repl ())
  else (
    print_string "May your trip be as enjoyable as finding ";
    print_endline "extra bananas at the bottom of the bag!")

let () =
  print_endline "Welcome to Monkey Islang!";
  print_endline "Type 'q;' to exit.";
  repl ()
