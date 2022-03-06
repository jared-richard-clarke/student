# LISP Interpreters

## Lexemes vs. Syntax
| Terminology | Lexical Grammar | Syntactic Grammer |
| ----------- | --------------- | ----------------- |
| alphabet    | characters      | tokens            |
| string      | lexeme or token | expression        |
| application | scanner         | parser            |

## Context-Free Grammar
- **derivations**: strings derived from rules of the grammar.
- **productions**: rules that produce strings.
- **head**: name of production.
- **body**: description of what a production generates.
- **terminal**: tokens from the scanner — "end points" in that they lead to no further moves in the game.
- **nonterminal**: a named reference to another rule in the grammar. Recursively-defined terminals allow finite productions to produce infinite strings.

## Scheme's Core Forms
Table of core forms pulled from [The Scheme Programming Language: Fourth Edition](https://www.scheme.com/tspl4/) by R. Kent Dybvig.
| Body                | Composition |
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
