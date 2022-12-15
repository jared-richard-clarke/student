# Haskell: Monadic Parsers

## Parser Monad

```haskell
result :: a -> Parser a
bind   :: Parser a -> (a -> Parser b) -> Parser b

 -- implementation of ...

result :: a -> m a
bind   :: m a -> (a -> m b) -> m b
```

A monad is type constructor `m`

### Sidenote

Monads must also satisfy these laws:

1. `return` is a left and right unit for `>>=`.
2. `>>=` is associative.

```text
           return a >>= f = f a
             p >>= return = p
p >>= (na -> (f a >>= g)) = (p >>= (na -> f a)) >>= g
```

## Monad

```haskell
-- A Parser wraps a function that takes a string as its argument, and returns a list of results.
newtype Parser a = Parser (String -> [(a, String)])

-- parse is a deconstructor function. It pulls the parser function out of the Parser data type.
parse (Parser p) = p

-- built-in class definition
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

-- Make parser instance of Monad
instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs]) 
```

> [T]he parser `p >>= f` first applies the parser `p` to the argument string `cs` 
> to give a list of results of the form `(a, cs')`, where `a` is a value and `cs'` 
> is a string. For each such pair, `(f a)` is a parser which is applied to the 
> string `cs'`. The result is a list of lists, which is then concatenated to 
> give the final list of results.
>
> **Parser Pearls** by Graham Hutton and Erik Meijer
