let () =
  print_endline "Simple calculator v0.1. Ctrl-D to quit";

  let open Pratt in
  let rec loop () =
    try
      print_string ">> ";
      (* Read *)
      read_line ()
      (* Eval *)
      |> Lexer.tokenize
      |> Parser.parse |> Evaluator.eval
      (* Print *)
      |> Printf.printf "%d\n";
      (* Loop *)
      loop ()
    with End_of_file -> ()
  in
  loop ()
