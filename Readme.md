# Orangutan

Whence Orangutan? In verity, we choose Orangutan because we are on Monkey Islang
and Orangutans are splendid great apes native to the verdant rain-forests of
Indonesia and Malaysia. Being engrossed in yonder task of writing an interpreter
of Monkey Lang in OCaml, it was natural to select a primate whose appellation doth
starts with an 'O'.

# Notes

## Setup env
- You can create a switch for the package
  - `opam switch create ./ 5.1.1`
  - `opam install dune alcotest -y`

## Run repl
- to run the repl: `dune exec orangutan`
``` bash
Welcome to Monkey Islang!
Type 'q;' to exit.
>> let a = 10;;
LET IDENT(a) ASSIGN INT(10) SEMICOLON SEMICOLON
>> if a == 10 then true else false;;
IF IDENT(a) EQ INT(10) THEN TRUE ELSE FALSE SEMICOLON SEMICOLON
>> q;
May your trip be as enjoyable as finding
extra bananas at the bottom of the bag!
```
