# LISP Interpreters

## Context-Free Grammar
- **derivations**: strings derived from rules of the grammar.
- **productions**: rules that produce strings.
- **head**: name of production.
- **body**: description of what a production generates.
- **terminal**: tokens from the scanner — "end points" for the grammar.
- **nonterminal**: a named reference to another rule in the grammar. A few rules containing multiple productions can produce combinatorially larger sets of strings. Recursively-defined rules can produce potentially infinite sets of strings.
- **parser**: maps tokens to terminals in the grammar to figure out which rules could have generated that string.

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
