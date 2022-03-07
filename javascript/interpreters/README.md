# JavaScript Interpreters

## Table of Contents
- `egg.js`: **Egg** is a language implemented by Marijn Haverbeke for his book *Eloquent JavaScript*. 
I included **Egg** because of its simplicity and similarity to **LISP**.

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
