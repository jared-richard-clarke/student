# LISP Interpreters

This repository contains a collection of meta-circular LISP interpreters and symbolic processors
as written by others and myself. I include examples from other languages if the code helps me better
understand the problem domain.

## Table of Contents
- `byrd.scm`: a minimal Scheme interpreter as defined by Bill Byrd.
- `compute.ml`: an OCaml arithmetic interpreter and symbolic processor as defined by **The OCaml Manual**
- `compute.scm`: an arithmetic interpreter and symbolic processor as defined by me.
- `dybvig.scm`: a Scheme interpreter as defined by R. Kent Dybvig.

## Grammar
**LISP** is defined in terms of the evaluation of data structures, **not** in terms of the syntax of character streams or files.
Most **LISP** programs begin life as text files. The reader parses text and produces the data structures for the interpreter or compiler. 
**LISP** data is, in many ways, like **JSON** or **XML**.

## REPL

Being one of the first high-level programming languages, **LISP** pioneered the **REPL**, the **Read-Evaluate-Print Loop**.

```scheme
(define (loop env)
  (print (eval env (read))))
  (loop env)
```

## Scheme's Core Forms
Table of core forms pulled from [The Scheme Programming Language: Fourth Edition](https://www.scheme.com/tspl4/) by R. Kent Dybvig.
| Head                | Body        |
| ------------------- | ----------- |
| program             | form\* |
| form                | definition \| expression |
| definition          | variable definition \| (`begin` definition\*) |
| variable definition | (`define` variable expression) |
| expression          | constant |
|                     | variable |
|                     | (`quote` datum) |
|                     | (`lambda` formals expression expression\*) |
|                     | (`if` expression expression expression) |
|                     | (`set!` variable expression) |
|                     | application |
| constant            | boolean \| number \| character \| string |
| formals             | variable |
|                     | (variable\*) |
|                     | (variable variable\* . variable) |
| application         | (expression expression\*) |
