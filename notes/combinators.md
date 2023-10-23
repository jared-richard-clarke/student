# Combinators

> Combinatory logic can be viewed as a variant of the lambda calculus, in which lambda expressions
> (representing functional abstraction) are replaced by a limited set of combinators, primitive
> functions without free variables.
>
> — **Combinatory Logic**, Wikipedia

All combinators can be expressed using only `K` and `S`.

```
I x     = x            -- The Idiot bird, or identity combinator
K x y   = x            -- The Kestrel, or constancy combinator
M x     = x >> x       -- The Mockingbird
T x y   = y x          -- The Thrush, or swap combinator
Q x y z = y (x z)      -- The Queer bird, or composition combinator
S x y z = x z (y z)    -- The Starling, or fusion combinator
-- and the infamous...
Y f x   = f (Y f) x    -- The Sage bird, or Y-combinator, or fixed-point combinator
Y f     = f (Y f)      -- Apply η reduction
```
