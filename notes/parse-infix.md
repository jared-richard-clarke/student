# Parsing Infix Arithmetic Expressions

Primary Source: **Software Tools in Pascal** by Brian W. Kernighan and P.J. Plauger

| head       | body                                                     |
| ---------- | -------------------------------------------------------- |
| expression | term \| term `+` term \| term `-` term                   |
| term       | factor \| factor `*` factor \| factor `/` factor         |
| factor     | number \| `(` expression `)` \| `+` factor \| `-` factor |
| number     | 0 \| 1 \| 2 \| 3 \| 4 \| 5 \| 6 \| 7 \| 8 \| 9           |

<figure>
  <img src="https://user-images.githubusercontent.com/80301412/166803499-7743f365-b36c-495e-930e-83e2970d0e15.png" alt=""/>
  <figcaption>
    Railroad diagrams specifying infix syntax for arithmetic expressions, terms, factors, and numbers.
  </figcaption>
</figure>
