# List Comprehensions

List comprehensions are syntactic sugar for processing lists in a monadic context.
The monadic context for lists is non-determinism.

## Haskell

```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
  fail _   = []

[(x, y) | x <- ['a', 'b'], y <- [1, 2]]

-- equivalent ->

do x <- ['a', 'b']
   y <- [1, 2]
   return (x, y)
   
-- equivalent ->

['a', 'b'] >>= \x ->
[1, 2]     >>= \y ->
return (x, y)

-- evaluates ->

[('a', 1), ('a', 2), ('b', 1), ('b', 2)]
```

## Process

```text
                ['a', 'b']
                /        \
       [1, 2]                [1, 2]
       /    \               /     \
['a', 1]    ['a', 2] ['b', 1]     ['b', 2]
```
