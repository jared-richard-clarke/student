# Haskell Monad

## Class Definition

```haskell
-- Haskell's built-in class definition for Monad. `m` is a type constructor.
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```

## Implementation: `Maybe`

```haskell
data Maybe a = Just a | Nothing

instance Monad Maybe where
    return x  = Just x
    m >>= g = case m of
                   Nothing -> Nothing
                   Just x  -> g x
    fail _ = []
```

## Implementation: `Parser`

```haskell
newtype Parser a = Parser (String -> [(a, String)])

-- deconstructor function.
parse (Parser p) = p

instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>= f  = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs]) 
```

## List Comprehensions

List comprehensions are syntactic sugar for generating lists in a monadic context.

```haskell
instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)
    fail _   = []

[1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)

-- equals...

listOfTuples = do
    n  <- [1,2]
    ch <- ['a','b']
    return (n,ch)

-- equals...

[ (n,ch) | n <- [1,2], ch <- ['a','b'] ]
```
