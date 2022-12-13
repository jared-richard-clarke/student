# Haskell Monad

```haskell
{- Haskell's built-in class definition for Monad.
  `m` is a type constructor. -}
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
 
 -- A parser implementation of a monad.
instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>=  f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])
```
