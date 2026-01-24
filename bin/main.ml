let () =
  Monkey.hello ();
  Printf.printf "> ";
  let line = read_line () in
  Printf.printf "echo> %s\n" line
