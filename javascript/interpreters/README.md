# JavaScript Interpreters

## Table of Contents
- `egg.js`: **Egg** is a language implemented by Marijn Haverbeke for his book *Eloquent JavaScript*. 
I included **Egg** because of its simplicity and similarity to **LISP**.
- `pratt-parser-crockford.md`: A pratt parser and accompanying lexer as implemented by Douglas Crockford.

## Lexemes vs. Syntax
| Terminology | Lexical Grammar | Syntactic Grammar |
| ----------- | --------------- | ----------------- |
| alphabet    | characters      | tokens            |
| string      | lexeme or token | expression        |
| application | scanner         | parser            |

## Context-Free Grammar
- **derivations**: strings derived from rules of the grammar.
- **productions**: rules that produce strings.
- **head**: name of production.
- **body**: description of what a production generates.
- **terminal**: tokens from the scanner — "end points" for the grammar.
- **nonterminal**: a named reference to another rule in the grammar. A few rules containing multiple productions can produce combinatorially larger sets of strings. Recursively-defined rules can produce potentially infinite sets of strings.

## Miscellaneous Definitions
- **parser**: maps tokens to terminals in the grammar to figure out which rules could have generated that string.
- **post-order traversal**: Each node evaluates its children before evaluating itself.

## The Metacircular Evaluator

> 1. To evaluate a combination (a compound expression other than a special form), evaluate the subexpressions and then apply the value of the operator subexpression  to the values of the operand subexpressions.
>
> 2. To apply a compound procedure to a set of arguments, evaluate the body of the procedure in a new environment. To construct this environment, extend the environment part of the procedure object by a frame in which the formal parameters of the procedure are bound to the arguments to which the procedure is applied.

— [The Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H-26.html), Chapter 4.1
