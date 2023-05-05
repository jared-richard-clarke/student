# Monads

Source: **Monads for Functional Programming** by Philip Wadler

## Monad: Lambda Notation

`m * λa.n` analogous to `let a = m in n`, where `m` and `n` are expressions and `a` is a variable.

Perform computation `m`, bind `a` to the resulting value, then perform computation `n`.

### Types:

- `m :: M a`
- `a :: a`
- `n :: M b`
- `λa.n :: a -> M b`
- `m * λa.n :: M b`

## Monad Laws

### Left Unit

`unit a * λb.n = n[a/b]`

### Right Unit

`m * λa.unit a = m`

### Associative

`m * (λa.n * λb.o) = (m * λa.n) * λb.o`
