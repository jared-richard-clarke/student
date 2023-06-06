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

-- deterministic parser

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

{-
  force: ensures `many` has the expected behavior under lazy evaluation.

  Given a parser `p` that always succeeds, the parser `force p` has the same behaviour
  as `p`, except that before any parsing of the input string is attempted the result of
  the parser is immediately forced to take on the form `(⊥,⊥):⊥`, where `⊥` represents
  a presently undefined value. This technique prevents the formation of large intermediate
  data structures.
-}

force :: Parser a -> Parser a
force p = \inp -> let x = p inp in
                  (fst (head x), snd (head x)) : tail x

many :: Parser a -> Parser [a]
many p = force ([x:xs | x <- p, xs <- many p] +++ [[]])

{-
  many :: Parser a -> Parser [a]
  many p = [x:xs | x <- p, xs <- many p] ++ [[]]
-}

{-
  `many` defined with the `++` combinator is non-deterministic so ...
  `(many letter) "No!"` -> `[("No", "!"), ("N", "o!"), ("", "No!")]`
  Non-determinism means both alternatives can be evaluated,
  even if the first alternative is successful.
  
  `many` and other repeating parsers can be redefined with `+++`, 
  the deterministic combinator, to improve parser efficiency.
-}

many1 :: Parser a -> Parser [a]
many1 p = [x:xs | x <- p, xs <- many p]

sepby :: Parser a -> Parser b -> Parser [a]
p `sepby` sep = (p `sepby1` sep) +++ [[]]

sepby1 :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep = [x:xs | x <- p,
		       , xs <- many [y | _ <- sep, y <- p]]

-- chain: repetition with meaningful separators — the separator 
-- usually being some kind of operation.

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v = (p `chainl1` op) +++ [v]

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p `bind` rest
                 where
                    rest x = (op `bind` \f ->
                              p  `bind` \y ->
                              rest (f x y)) +++ [x]

chainr :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v = (p `chainr1` op) +++ [v]

chainr1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op =
    p `bind` \x ->
        [f x y | f <- op, y <- p `chainr1` op] +++ [x]

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
nat = [ord x - ord '0' | x <- digit] `chainl1` [op]
      where
        m `op` n = 10*m + n

{-
=== equivalent ===
nat :: Parser Int
nat = [eval xs | xs <- many1 digit]
      where
        eval xs = foldl1 op [ord x - ord '0' | x <- xs]
        m `op` n = 10*m + n
-}

int :: Parser Int
int = [f n | f <- op, n <- nat]
      where
        op = [negate | _ <- char '-'] ++ [id]

bracket :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = [x | _ <- open, x <- p, _ <- close]

-- lexers: whitespace, comments, and keywords
spaces :: Parser ()
spaces = [() | _ <- many1 (sat isSpace)]
         where
           isSpace x =
           (x == ' ') || (x == '\n') || (x == '\t')
	   
comment :: Parser ()
comment = [() | _ <- string "--"
              , _ <- many (sat (\x -> x /= '\n'))]

junk :: Parser ()
junk = [() | _ <- many (spaces +++ comment)]

parse :: Parser a -> Parser a
parse p = [v | _ <- junk, v <- p]

token :: Parser a -> Parser a
token p = [v | v <- p, _ <- junk]

-- complete parsers

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

identifier :: [String] -> Parser String
identifier ks = token [x | x <- ident, not (elem x ks)]
```

## The Parser Type Revisited

The parser monad — as implemented by Graham Hutton and Erik Meijer — can be defined in terms
of two simpler monads: the list monad (non-deterministic computations) and the state monad 
(stateful computations).

**Side Note**: `State` describes functions that consume a state and produce both a result and 
an updated state, which are given back in a tuple — usually `(a, s)`, where `a` is the result
and `s` is the state. `State` automates the threading of state between functions.

```haskell
-- non-deterministic ---> |-----------|
-- stateful --> |---------------------| <- a.k.a: state processor
-- state --------------------> |----|
type Parser a = String -> [(a, String)]

-- - generalizes ->

type StateM m s a = s -> m (a, s)
```

### Parser Redefined

```haskell
type StateM m s a = s -> m (a, s)

instance Monad m => Monad (StateM m s) where
  result v = \s -> result (v, s)
  stm `bind` f = \s -> stm s `bind` \(v, s') -> f v s'

instance MonadOPlus m => MonadOPlus (StateM m s) where
  zero = \s -> zero
  stm ++ stm' = \s -> stm s ++ stm' s

instance Monad m => StateMonad (StateM m s) s where
  update f = \s -> result (s, f s)

-- Non-deterministic computations that can return many results.
type Parser a = StateM [] String a

{-
  Alternatively:
  data Maybe a = Just a | Nothing
  type Parser a = StateM Maybe String a
-}

-- The advantage of the monadic definition of `item` is that it does not depend upon
-- the internal details of the `Parser` type.

item = [x | (x : _) <- update tail]
```
