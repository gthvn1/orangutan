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
{Token(LET, "let")}
{Token(IDENT, "add")}
{Token(ASSIGN, "=")}
{Token(FUNCTION, "fn")}
{Token(LPAREN, "(")}
{Token(IDENT, "x")}
{Token(COMMA, ",")}
{Token(IDENT, "y")}
{Token(RPAREN, ")")}
{Token(LBRACE, "{")}
{Token(IDENT, "x")}
{Token(PLUS, "+")}
{Token(IDENT, "y")}
{Token(SEMICOLON, ";")}
{Token(RBRACE, "}")}
{Token(SEMICOLON, ";")}
>> Bye
```

# State

- We are at page 28/210 of "Writing An Interpreter In Go"
  - Chapter2: Parsing
