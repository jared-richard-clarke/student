# Haskell Monad

## Class Definition

```haskell
-- Haskell's built-in class definition for Monad. `m` is a type constructor.
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```

## Parser Implementation

```haskell
 -- A parser implementation of a monad.
instance Monad Parser where
    return a = Parser (\cs -> [(a, cs)])
    p >>=  f = Parser (\cs -> concat [parse (f a) cs' | (a, cs') <- parse p cs])
    
-- parse is a deconstructor function. It pulls the parser function out of the Parser data type.
parse (Parser p) = p
```
> [T]he parser `p >>= f` first applies the parser `p` to the argument string `cs` 
> to give a list of results of the form `(a, cs')`, where `a` is a value and `cs'` 
> is a string. For each such pair, `(f a)` is a parser which is applied to the 
> string `cs'`. The result is a list of lists, which is then concatenated to 
> give the final list of results.
>
> **Parser Pearls** by Graham Hutton and Erik Meijer
