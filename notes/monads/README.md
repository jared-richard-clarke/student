# Monads

A collection of notes on the tricky subject of monads.

## Haskell Definition

A monad is defined by three things:
1. A type constructor `m`.
2. A function `return`.
3. An operator `(>>=)`, which is pronounced "bind".

### The Three Monad Laws

```haskell
m >>= return     =  m                        -- right unit
return x >>= f   =  f x                      -- left unit

(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity
```

## Uses

> "[M]onads are by no means limited to input and output. 
>  They can be used to provide a whole range of features, such as exceptions, 
>  state, non-determinism, continuations, coroutines, and more."
>
> — [Haskell Wiki: Understanding Monads](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)

## Monads and Equivalent Imperative Semantics

| Monad    | Imperative                |
| -------- | ------------------------- |
| `Maybe`  | Exception (anonymous)     |
| `Either` | Exception (error message) |
| `IO`     | Input / Output            |
| `[]`     | Nondeterminism            |
| `Reader` | Environment               |
| `Writer` | Logger                    |
| `State`  | Global State              |
