# Orangutan

Whence Orangutan? In verity, we choose Orangutan because we are on
[Monkey](https://monkeylang.org/) Islang and Orangutans are splendid great
apes native to the verdant rain-forests of Indonesia and Malaysia.
Being engrossed in yonder task of writing an interpreter of Monkey Lang in
OCaml, it was natural to select a primate whose appellation doth starts with
an 'O'.

- Test: `dune runtest`

- Debug is enabled by default so you should redirect error. Run:
```sh
❯ dune exec orangutan 2>/tmp/debug
Hello, This is the Monkey programming language!
Feel free to type in commands, Ctrl-D to quit.
>> let add = fn(x,y) {x + y; };
{Type:"LET" Literal:"let"}
{Type:"IDENT" Literal:"add"}
{Type:"ASSIGN" Literal:"="}
{Type:"FUNCTION" Literal:"fn"}
{Type:"LPAREN" Literal:"("}
{Type:"IDENT" Literal:"x"}
{Type:"COMMA" Literal:","}
{Type:"IDENT" Literal:"y"}
{Type:"RPAREN" Literal:")"}
{Type:"LBRACE" Literal:"{"}
{Type:"IDENT" Literal:"x"}
{Type:"PLUS" Literal:"+"}
{Type:"IDENT" Literal:"y"}
{Type:"SEMICOLON" Literal:";"}
{Type:"RBRACE" Literal:"}"}
{Type:"SEMICOLON" Literal:";"}
>> Bye
```

# State

- We are at page 28/210 of [Writing An Interpreter In Go](https://interpreterbook.com/)
  - Chapter2: Parsing
