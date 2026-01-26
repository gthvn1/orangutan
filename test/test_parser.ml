open Alcotest

let test_ping_parser () =
  check string "ping-pong" "pong" (Monkey.Parser.ping ())

let tests = [ test_case "Ping parser" `Quick test_ping_parser ]
