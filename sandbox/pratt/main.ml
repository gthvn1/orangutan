let () =
  let _p : Pratt.Token.t = Pratt.Token.Plus in
  Pratt.to_tokens "123" |> ignore;
  print_endline "Hello from pratt"
