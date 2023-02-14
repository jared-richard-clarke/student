# Parsec Types

A small sample of types in **Parsec** that implement consumer-based parsing as described in
**Parsec: Direct Style Parser Combinators For The Real World** by Daan Leijen and Erik Meijer

```haskell
newtype ParsecT s u m a
    = ParsecT {unParser :: forall b .
                 State s u
              -> (a -> State s u -> ParseError -> m b) -- consumed ok
              -> (ParseError -> m b)                   -- consumed err
              -> (a -> State s u -> ParseError -> m b) -- empty ok
              -> (ParseError -> m b)                   -- empty err
              -> m b
             }
{-
  s = stream type
  u = user state type
  m = underlying monad
  a = return type
-}

type Parsec s u = ParsecT s u Identity

data Consumed a = Consumed a | Empty !a

data Reply s u a = Ok a !(State s u) ParseError | Error ParseError

data State s u State {
    stateInput :: s,
    statePos   :: !SourcePos,
    stateUser  :: !u
}
```
