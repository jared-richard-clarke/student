# Parsing Infix Arithmetic Expressions

Primary Source: **Software Tools in Pascal** by Brian W. Kernighan and P.J. Plauger

| head       | body                                                     |
| ---------- | -------------------------------------------------------- |
| expression | term \| term `+` term \| term `-` term                   |
| term       | factor \| factor `*` factor \| factor `/` factor         |
| factor     | number \| `(` expression `)` \| `+` factor \| `-` factor |
| number     | 0 \| 1 \| 2 \| 3 \| 4 \| 5 \| 6 \| 7 \| 8 \| 9           |
