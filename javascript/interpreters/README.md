# Interpreters
My growing collection of small interpreters implemented in JavaScript.

## Table of Contents
- `egg.js`: **Egg** is a language implemented by Marijn Haverbeke for his book *Eloquent JavaScript*. 
I included **Egg** because of its simplicity and similarity to **LISP**.

## Recursive Descent Parsing
A broad representation of a recursive-descent parser.
| grammar | code |
| ------- | ---- |
| terminal | code to match and consume a token |
| nonterminal | call to that rule's function |
| \| | `if` or `switch` statement |
| `*` or `+` | `while` or `for` loop |
| `?` | `if` statement |
