# Simple Arithmetic compiler & Stack VM

This project is a small educational compiler for arithmetic expressions,
written in OCaml. It parses, compiles, and evaluates integer expressions
with addition, subtraction, multiplication, division, and unary negation.

Example expressions it can handle:
```
+5 - (-2)
3 * (4 + 1)
-7 + 2 * 3
```

## Project structure

- `lexer.ml` — Tokenizes input strings into arithmetic tokens.
- `parser.ml` — Parses tokens into an abstract syntax tree (AST) using a Pratt parser.
- `compiler.ml` — Compiles the AST into **stack-based bytecode**.
- `stack_vm.ml` — Executes the bytecode on a simple **stack-based virtual machine**.

Future plans include generating **x86_64 assembly** from the same bytecode and producing
native executables.

## Example usage

```OCaml
dune utop
🐫 > open Pratt;;
🐫 > let input = "5 + 2 * (3 - 1)";;
val input : string = "5 + 2 * (3 - 1)"
🐫 > let tokens = Lexer.tokenize input;;
val tokens : Lexer.Token.t list =
  [Pratt.Lexer.Token.Int 5; Pratt.Lexer.Token.Plus; Pratt.Lexer.Token.Int 2;                                             Pratt.Lexer.Token.Mult; Pratt.Lexer.Token.Lparen; Pratt.Lexer.Token.Int 3;                                            Pratt.Lexer.Token.Minus; Pratt.Lexer.Token.Int 1; Pratt.Lexer.Token.Rparen]
🐫 > let ast = Parser.parse tokens;;                                                                                  val ast : Parser.expr =
  Pratt.Parser.Add (Pratt.Parser.Int 5,
   Pratt.Parser.Mul (Pratt.Parser.Int 2,
    Pratt.Parser.Sub (Pratt.Parser.Int 3, Pratt.Parser.Int 1)))
🐫 > let bytecode = Compiler.to_bytecode ast;;
val bytecode : Stack_vm.insn list =
  [Pratt.Stack_vm.Push 5; Pratt.Stack_vm.Push 2; Pratt.Stack_vm.Push 3;
   Pratt.Stack_vm.Push 1; Pratt.Stack_vm.Sub; Pratt.Stack_vm.Mul;
   Pratt.Stack_vm.Add]
🐫 > let result = Stack_vm.run bytecode;;
val result : int = 9
🐫 >
```
