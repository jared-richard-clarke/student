# Combinator Parsers in Gofer

**Monadic Parser Combinators** by Graham Hutton and Erik Meijer

The code below is Gofer (Good For Equational Reasoning), a now deprecated
implementation of the Haskell programming language. Its syntax is closer
to Miranda. Gofer was eventually replaced by Hugs, which would eventually 
be deprecated as well.
  
**Side Note:** In true Gofer code, type synonyms must be supplied all their
arguments. The `Monad` instance declaration for `result` is therefore invalid.

> "In the first versions of Haskell, the comprehension syntax was available 
>  for all monads. Later the comprehension syntax was restricted to lists."
>  
>  — [Haskell Wiki](https://wiki.haskell.org/List_comprehension)

## About

The basic idea behind monads is to distinguish the values that a computation can 
produce from the computation itself. We can think of `m a` as the type of computations 
the yield results of type `a`, with the nature of the computation captured by the type 
constructor `m`. The combinators `result` and `bind` (with potentially `zero` and `(++)`)
provide a means to structure the building of such computations.

```haskell
result :: m a
bind   :: m a -> (a -> m a) -> m a
zero   :: m a
(++)   :: m a -> m a -> m a
```

## Parsers

```haskell
-- Parser is a function that inputs a string and outputs a list 
-- of zero or more (value, string) pairs.
type Parser a = String -> [(a, String)]

-- primitives

item :: Parser Char
item = \inp -> case inp of
                   []     -> []
                   (x:xs) -> [(x,xs)]

{-
  For "bind", the parser "p" is applied to the input string, yielding 
  a list of (value, string) pairs. Since "f" is a function that takes
  a value and evaluates to a parser, it can be applied to each value
  (and unconsumed input string) in turn. This results in a list of lists 
  of (value, string) pairs, that can then be flattened to a single list 
  using "concat".
-}

instance Monad Parser where
  -- result :: a -> Parser a
  result v   = \inp -> [(v, inp)]
  -- bind :: Parser a -> (a -> Parser b) -> Parser b
  p `bind` f = \inp -> concat [f v out | (v, out) <- p inp]

instance MonadOPlus Parser where
  -- Parser a
  zero   = \inp -> []
  -- Parser a -> Parser a -> Parser a
  p ++ q = \inp -> (p inp ++ q inp)

-- deterministic parsers

first :: Parser a -> Parser a
first p = \inp -> case p inp of
                    [] -> []
                    (x:xs) -> [x]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = first (p ++ q)

-- combinators

sat :: (Char -> Bool) -> Parser Char
sat p = [x | x <- item, p x]

{-
  === equivalent ===
  sat :: (Char -> Bool) -> Parser Char
  sat p = item `bind` \x ->
          if p x then result x else zero
-}

many :: Parser a -> Parser [a]
many p = [x:xs | x <- p, xs <- many p] ++ [[]]

{-
  "++" is non-deterministic so ...
  (many letter) "No!" -> [("No", "!"), ("N", "o!"), ("", "No!")]
  Non-determinism means both alternatives can be evaluated,
  even if the first alternative is successful.
-}

many1 :: Parser a -> Parser [a]
many1 p = [x:xs | x <- p, xs <- many p]

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) ++ [[]]

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = [x:xs | x <- p,
		       , xs <- many [y | _ <- sep, y <- p]]

-- evaluating expressions

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = (p `chainl1` op) ++ [v]

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p `bind` rest
                 where
                    rest x = (op `bind` \f ->
                              p  `bind` \y ->
                              rest (f x y)) ++ [x]

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = (p `chainr1` op) ++ [v]

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op =
    p `bind` \x ->
        [f x y | f <- op, y <- p `chainr1` op] ++ [x]

ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 (++) [[op | _ <- p] | (p, op) <- xs]

-- parsers

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat (\x -> '0' <= x && x <= '9')

lower :: Parser Char
lower = sat (\x -> 'a' <= x && x <= 'z')

upper :: Parser Char
upper = sat (\x -> 'A' <= x && x <= 'Z')

letter :: Parser Char
letter = lower ++ upper

alphanum :: Parser Char
alphanum = letter ++ digit

string :: String -> Parser String
string ""     = [""]
string (x:xs) = [x:xs | _ <- char x, _ <- string xs]

{-
  === equivalent ===
  string :: String -> Parser String
  string "" = result ""
  string (x:xs) = char x    `bind` \_ ->
                  string xs `bind` \_ ->
                  result (x:xs)
-}

ident :: Parser String
ident = [x:xs | x <- lower, xs <- many alphanum]

nat :: Parser Int
nat = [eval xs | xs <- many1 digit]
      where
        eval xs = foldl1 op [ord x - ord '0' | x <- xs]
        m `op` n = 10*m + n

int :: Parser Int
int = [f n | f <- op, n <- nat]
      where
        op = [negate | _ <- char '-'] ++ [id]

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = [x | _ <- open, x <- p, _ <- close]
```
