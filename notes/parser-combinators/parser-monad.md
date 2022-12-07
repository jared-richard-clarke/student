# Haskell: Monadic Parsers

## Parser Monad

```haskell
result :: a -> Parser a
bind   :: Parser a -> (a -> Parser b) -> Parser b

 -- otherwise

result :: a -> M a
bind   :: M a -> (a -> M b) -> M b
```

A monad is type constructor `M`

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
newtype Parser a = Parser (String -> [(a, String)])

-- built-in class definition
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b

-- Make parser instance of Monad
instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | 
                                  (a, cs') <- parse p cs])
```
