# Parser Combinators

> "In computer programming, a parser combinator is a higher-order function that accepts 
>  several parsers as input and returns a new parser as its output. In this context, a parser 
>  is a function accepting strings as input and returning some structure as output, typically 
>  a parse tree or a set of indices representing locations in the string where parsing stopped 
>  successfully. Parser combinators enable a recursive descent parsing strategy that facilitates 
>  modular piecewise construction and testing. This parsing technique is called combinatory parsing." 
>
> — Wikipedia, **Parser Combinator**

> "Monadic combinator parsers consist of a monad `Parser a` (typically of the form
>  `String -> Result a` for some functor `Result`) with a unit `return` and bind
>  (`>>=`) operation, and a number of parser specific operations, usually a choice
>  combinator (`<|>`) and a function `satisfy` for constructing elementary parsers
>  for terminal symbols."
>
> — Daan Leijen and Erik Meijer, **Parsec: Direct Style Monadic Parser Combinators For The Real World**

> "In functional programming, a popular approach to building recursive descent parsers
>  is to model parsers as functions, and to define higher-order functions (or combinators) 
>  that implement grammar constructions such as sequencing, choice, and repetition."
>
>  — Graham Hutton and Erik Meijer, **Monadic Parser Combinators**

## Monad Laws

```haskell
-- === Monad Laws ===
-- === left unit ===
return a >>= f = f a

-- === right unit ===
p >>= return = p

-- === associative ===
p >>= (\a -> (f a >>= g)) = (p >>= (\a -> f a)) >>= g

-- === MonadZero and MonadPlus Laws ===
-- === left unit ===
zero ++ p = p

-- === right unit ===
p ++ zero = p

-- === associative ===
p ++ (q ++ r) = (p ++ q) ++ r

-- === Special Case: Parsers ===
-- === left unit ===
zero >>= f = zero

-- === right unit ===
p >>= const zero = zero

-- === right distributive ===
(p ++ q) >>= f = (p >>= f) ++ (q >>= f)

-- === associative ===
p >>= (\a -> f a ++ g a) = (p >>= f) ++ (p >>= g)
```
