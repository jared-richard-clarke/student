# Parsing Infix Arithmetic Expressions

Primary Source: [Crafting Interpreters](https://craftinginterpreters.com/parsing-expressions.html)
by Robert Nystrom

| name       | operators         | Associates |
| ---------- | ----------------- | ---------- |
| term       | `-` `+`           | left       |
| factor     | `/` `*`           | left       |
| unary      | `-`               | right      |

Each rule only matches at its precedence level or higher. 
A term can match both `1 + 6` and `3 * 4 - 1`.

| head       | body                                                            |
|----------- | --------------------------------------------------------------- |
| term       | factor ( ( `-` \| `+` ) factor )\*                              |
| factor     | unary ( ( `/` \| `*` ) unary )\*                                |
| unary      | ( `-` ) unary \| primary                                        |
| primary    | NUMBER \| `(` term `)`                                          |

A recursive descent parser is a literal translation of the grammar rules. 

| grammar     | code                              |
| ----------- | --------------------------------- |
| terminal    | code to match and consume a token |
| nonterminal | call to that rule's function      |
| \|          | `if` or `switch` statement        |
| `*` or `+`  | `while` or `for` loop             |
| `?`         | `if` statement                    |
