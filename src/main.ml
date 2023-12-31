let rec repl () =
  print_string ">> ";
  let line = read_line () in
  if String.compare "q;" line <> 0 then (
    print_endline line;
    repl ())
  else (
    print_endline "May your trip be as enjoyable as finding";
    print_endline "extra bananas at the bottom of the bag!")

let () =
  print_endline "Welcome to Monkey Islang!";
  print_endline "Type 'q;' to exit.";
  repl ()
