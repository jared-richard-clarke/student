# Parsing Arithmetic Expressions

A simple arithmetic parser as implemented in **Functional Pearls**
by Graham Hutton and Erik Meijer

## Expression Grammar

```text
expr   ::= expr add-op term   | term
term   ::= term mul-op factor | factor
factor ::= digit | ( expr )
digit  ::= 0 | 1 | 2 ... 9
add-op  ::= + | -
mul-op  ::= * | /
```

## Parser and Evaluator

```haskell
expr  :: Parser Int
addop :: Parser (Int -> Int -> Int)
mulop :: Parser (Int -> Int -> Int)

expr = term `chainl1` addop

term = factor `chainl1` mulop

factor = digit +++ do {symb "("; n <- expr; symb ")"; return n}

digit = do {x <- token (sat isDigit); return (ord x - ord '0')}

addop = do {symb "+"; return (+)} +++ do {symb "-"; return (-)}

mulop = do {symb "*"; return (*)} +++ do {symb "/"; return (div)}

-- apply expr "1 - 2 * 3 + 4" -> [(-1, "")]
```
