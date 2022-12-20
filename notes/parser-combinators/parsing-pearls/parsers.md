# Parser Combinators

The following code is a series of parser combinators as implemented in
**Functional Pearls** by Graham Hutton and Erik Meijer

```haskell
-- Parser wraps a function that inputs a string
-- and outputs a list of results — a pair of type "a"
-- and the remaining unparsed string.
-- An empty list denotes failure

newtype Parser a = Parser (String -> [(a, String)])

-- unconditional parsing

item :: Parser Char
item = Parser (\cs -> case cs of
                        ""     -> []
                        (c:cs) -> [(c, cs)])

-- Pulls function out of Parser.

parse (Parser p) = p

-- class Monad m where
--    return :: a -> m a
--    (>>=) :: m a -> (a -> m b) -> m b

instance Monad Parser where
   return a = Parser (\cs -> [(a,cs)])
   p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

-- class Monad m => MonadZero m where
--    zero :: m a

instance MonadZero Parser where
   zero = Parser (\cs -> [])

-- class MonadZero m => MonadPlus m where
--    (++) :: m a -> m a -> m a

instance MonadPlus Parser where
   p ++ q = Parser (\cs -> parse p cs ++ parse q cs)

-- deterministic "choice" operator

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p ++ q) cs of
                            []     -> []
                            (x:xs) -> [x])

-- Creates a parser that consumes characters conditionally.

sat :: (Char -> Bool) -> Parser Char
sat p = do {c <- item; if p c then return c else zero}

char :: Char -> Parser Char
char c = sat (c ==)

-- Parse a specific string

string :: String -> Parser String
string "" = return ""
string (c:cs) = do {char c; string cs; return (c:cs)}

-- Parse repeated applications of a parser.

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

-- Parse repeated applications of parser separated
-- by parser "sep" whose results are thrown away.

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ return []

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = do a <- p
                    as <- many (do {sep; p})
                    return (a:as)

-- Parse repeated applications of a parser "p", separated
-- by applications of a parser "op" whose result value is
-- an operator that is assumed to associate to the left,
-- and which is used to combine the results from the "p" parsers.

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b))
                             +++ return a

-- Lexical combinators

space :: Parser String
space = many (sat isSpace)

-- Parse token using parser "p", throwing away any trailing space.

token :: Parser a -> Parser a
token p = do {a <- p; space; return a}

symb :: String -> Parser String
symb cs = token (string cs)

-- Apply parser "p", throwing away any leading space.

apply :: Parser a -> String -> [(a,String)]
apply p = parse (do {space; p})
```
