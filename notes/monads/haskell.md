# Haskell Monad

## Class Definition

```haskell
-- Haskell's built-in class definition for Monad. `m` is a type constructor.
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
```
