# Interpreters

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
