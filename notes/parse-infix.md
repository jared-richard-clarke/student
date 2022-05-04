# Parsing Infix Arithmetic Expressions

Primary Sources: 
- [Crafting Interpreters](https://craftinginterpreters.com/parsing-expressions.html) by Robert Nystrom
- **Software Tools in Pascal** by Brian W. Kernighan and P.J. Plauger

## Recursive Descent
Every grammar rule has an associated function.
Each function produces a syntax tree and returns it to the caller.
The body of a rule that contains a reference to another rule calls that other rule's function. Recursive calls produce a tree data structure.

## Infix Grammar
| head       | body                                                     |
| ---------- | -------------------------------------------------------- |
| expression | term \| term `+` term \| term `-` term                   |
| term       | factor \| factor `*` factor \| factor `/` factor         |
| factor     | number \| `(` expression `)` \| `+` factor \| `-` factor |
| number     | 0 \| 1 \| 2 \| 3 \| 4 \| 5 \| 6 \| 7 \| 8 \| 9           |

## Railroad Diagrams
<figure>
  <img src="https://user-images.githubusercontent.com/80301412/166803499-7743f365-b36c-495e-930e-83e2970d0e15.png" alt=""/>
  <figcaption>
    Railroad diagrams specifying infix syntax for arithmetic expressions, terms, factors, and numbers.
  </figcaption>
</figure>
