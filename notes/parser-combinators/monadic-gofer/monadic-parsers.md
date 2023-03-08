# Parsers in Gofer

**Monadic Parser Combinators** by Graham Hutton and Erik Meijer

The code below is Gofer (Good For Equational Reasoning), a now deprecated
implementation of the Haskell programming language. Its syntax is closer
to Miranda. Gofer was eventually replaced by Hugs, which would eventually 
be deprecated as well.
  
**Side Note:** In true Gofer code, type synonyms must be supplied all their
arguments. The `Monad` instance declaration for `result` is therefore invalid.
```haskell
type Parser a = String -> [(a, String)]

-- primitive parsers

instance Monad Parser where
  result v   = \inp -> [(v, inp)]
  p `bind` f = \inp -> concat [f v out | (v, out) <- p inp]

instance MonadOPlus Parser where
  zero   = \inp -> []
  p ++ q = \inp -> (p inp ++ q inp)

-- parser combinators

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v, inp') <- p inp]

sat :: (Char -> Bool) -> Parser Char
sat p = [x | x <- item, p x]

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
letter = lower `plus` upper

alphanum :: Parser Char
alphanum = letter `plus` digit

string :: String -> Parser String
string ""     = [""]
string (x:xs) = [x:xs | _ <- char x, _ <- string xs]

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

{-
=== expands ===
string :: String -> Parser String
string "" = result ""
string (x:xs) = char x    `bind` \_ ->
                string xs `bind` \_ ->
                result (x:xs)
-}

many :: Parser a -> Parser [a]
many p = [x:xs | x <- p, xs <- many p] ++ [[]]

many1 :: Parser a -> Parser [a]
many1 p = [x:xs | x <- p, xs <- many p]

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) ++ [[]]

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = [x:xs | x <- p,
		       , xs <- many [y | _ <- sep, y <- p]]
```
