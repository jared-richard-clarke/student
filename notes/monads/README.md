# Monads

A collection of notes on the tricky subject of monads.

## Haskell Definition

A monad is defined by three things:
1. A type constructor `m`.
2. A function `return`.
3. An operator `(>>=)`, which is pronounced "bind".

## Uses

> "[M]onads are by no means limited to input and output. 
>  They can be used to provide a whole range of features, such as exceptions, 
>  state, non-determinism, continuations, coroutines, and more."
>
> — [Haskell Wiki: Understanding Monads](https://en.wikibooks.org/wiki/Haskell/Understanding_monads)
