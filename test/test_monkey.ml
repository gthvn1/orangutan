open Alcotest

let test_add () = check int "same int" 3 (Monkey.add 1 2)

let () =
  run "Monkey test" [ ("add", [ test_case "1 + 2 = 3" `Quick test_add ]) ]
