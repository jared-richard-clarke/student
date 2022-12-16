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

return :: a -> Maybe a
return x  = Just x

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) m g = case m of
               Nothing -> Nothing
               Just x  -> g x
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
