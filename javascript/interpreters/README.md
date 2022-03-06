# Interpreters
My growing collection of small interpreters implemented in JavaScript.

## Table of Contents
- `egg.js`: **Egg** is a language implemented by Marijn Haverbeke for his book *Eloquent JavaScript*. 
I included **Egg** because of its simplicity and similarity to **LISP**.

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
- **nonterminal**: a named reference to another rule in the grammar. Few rules containing multiple productions can produce combinatorially larger sets of strings. Recursively-defined rules can produce potentially infinite sets strings.
