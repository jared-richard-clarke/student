# Monads in Haskell

A monad in Haskell is defined by three elements:
1. A type constructor `m`.
2. A function `return`.
3. An operator `(>>=)`, which is pronounced "bind".

### The Three Monad Laws

The monad laws are not so much laws as generally-agreed-upon rules for predictable
behaviors among different monad implementations.

```haskell
m >>= return     =  m                        -- right unit
return x >>= f   =  f x                      -- left unit

(m >>= f) >>= g  =  m >>= (\x -> f x >>= g)  -- associativity
```

**Right and Left Unit** ensure `return` is a neutral element in that it doesn't perform any computation.

**Associativity** ensures `>>=` cares only about the order of computations, not their nesting.

## Categorical Definition

Category Theory treats monads as functors with two additional combinators.

```haskell
fmap   :: (a -> b) -> M a -> M b  -- functor

return :: a -> M a
join   :: M (M a) -> M a          -- similar to concat for lists, where concat :: [[a]] -> [a]

-- bind can be defined as ...
m >>= g = join (fmap g m)

-- likewise ...
fmap f x = x >>= (return . f)
join x   = x >>= id
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

## Defining Functors, Applicatives, and Monads

Functor and Applicative must be defined for an instances of Monad.
You can implement types moving down the class hierarchy from
Functor to Applicative to Monad.

```haskell
instance Functor M where
    fmap = -- etc.

instance Applicative M where
    pure = -- etc.
    (<*>) = -- etc.

instance Monad M where
    (>>=) = -- etc.
```

Coversely, Functor and Applicative can be derived from Monad.

```haskell
instance Monad M where
    return = -- etc.
    (>>=) = -- etc.

instance Applicative M where
    pure = return
    (<*>) = ap

instance Functor M where
    fmap = liftM
```
