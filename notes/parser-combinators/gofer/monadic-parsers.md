# Monadic Parsers

Notes taken while reading...

**Monadic Parser Combinators**: Technical report NOTTCS-TR-96-4, Department of Computer Science, University of Nottingham, 1996

by Graham Hutton and Erik Meijer

## Side Note

This parser implementation is implemented in Gofer, a teaching language similar to Haskell.

## Monad Comprehension Syntax

```text
p1 `bind` \x1 ->
p2 `bind` \x2 ->
...
pn `bind` \xn ->
result (f x1 x2 ... xn)

Is equivalent to ...

do x1 <- p1
   x2 <- p2
   ...
   xn <- pn
   return (f x1 x2 ... xn)

Is equivalent to ...

[ f x1 x2 ... xn | x1 <- p1
                 , x2 <- p2
                 , ...
                 , xn <- pn ]
```

## Parsers

```Haskell
-- Parser Type

type Parser a = String -> [(a, String)]

-- Primitive Parsers

instance Monad Parser where
    -- result :: a -> Parser a
    result v = \input -> [(v, input)]
    -- bind :: Parser a -> (a -> Parser a) -> Parser b
    bind p f = \input -> concat [f v input' | (v, input') <- p input]

instance MonadOPlus Parser where
    -- zero :: Parser a
    zero = \input -> []
    -- (++) :: Parser a -> Parser a -> Parser a
    p ++ q = \input -> (p input ++ q input)

item :: Parser Char
item = \input -> case input of
                    []     -> []
                    (x:xs) -> [(x, xs)]

-- Parser Combinators

sat :: (Char -> Bool) -> Parser Char
sat predicate = [ x | x <- item, predicate x ]

{-
   === sat without monad comprehension syntax ===
   sat :: (Char -> Bool) -> Parser Char
   sat predicate = item `bind` \x ->
                   if predicate x then result x else zero
-}

many :: Parser a -> Parser [a]
many p = [ x:xs | x <- p, xs <- many p ] ++ [[]]

many1 :: Parser a -> Parser [a]
many1 p = [ x:xs | x <- p, xs <- many p ]

sepby1 :: Parser a -> Parser b -> Parser [a]
sepby1 p sep = [ x:xs | x  <- p
                      , xs <- many [ y | _ <- sep, y <- p ]]

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = (p `sepby1` sep) ++ [[]]

-- Parsers

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
string "" = [""]
string (x:xs) = [ x:xs | _ <- char x, _ <- string xs ]

{-
   === string without monad comprehension syntax ===
   string :: String -> Parser String
   string ""     = result ""
   string (x:xs) = char x    ‘bind‘ \_ ->
                   string xs ‘bind‘ \_ ->
                   result (x:xs)
-}

ident :: Parser String
ident = [ x:xs | x <- lower, xs <- many alphanum ]

nat :: Parser Int
nat = [ eval xs | xs <- many1 digit ]
        where
           eval xs = foldl1 op [ ord x - ord '0' | x <- xs ]
           m `op` n = 10*m + n

int :: Parser Int
int = [ f n | f <- op, n <- nat ]
      where
          op = [ negate | _ <- char '-' ] ++ [id]

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = [ x | _ <- open, x <- p, _ <- close ]

ints = bracket (char '[')
               (int `sepby1` char ',')
               (char ']')
```
