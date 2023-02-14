# Consumer-Based Parsing

Source: **Parsec: Direct Style Parser Combinators For The Real World** by Daan Leijen and Erik Meijer

## Basic Combinators

```haskell
type Parser a = String -> Consumed a

data Consumed a = Consumed (Reply a) | Empty (Reply a)

data Reply a = Ok a String | Error

return :: a -> Parser a
return x = \input -> Empty (Ok x input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy test = \input -> case input of
                 [] -> Empty Error
                 (c:cs) | test c    -> Consumed (Ok c cs)
                        | otherwise -> Empty Error
```

## `>>=`: input consumption

> "Due to laziness, a parser (p >>= f) directly returns with a Consumed constructor
>  if p consumes input. The computation of the final reply is delayed."
>  
>   — Daan Leijen and Erik Meijer

```haskell
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = 
  \input -> case p input of
    Empty reply1
      -> case reply1 of
           Ok x rest -> f x rest
           Error     -> Empty Error
    Consumed reply1
      -> Consumed
         (case reply1 of
            Ok x rest
              -> case f x rest of
                   Consumed reply2 -> reply2
                   Empty reply2    -> reply2
            error -> error
         )
```

| `p`        | `q`        | `p >>= q`  |
| ---------- | ---------- | ---------- |
| `Empty`    | `Empty`    | `Empty`    |
| `Empty`    | `Consumed` | `Consumed` |
| `Consumed` | `Empty`    | `Consumed` |
| `Consumed` | `Consumed` | `Consumed` |

## Choice

> "An LL(1) choice combinator only looks at its second alternative if the first hasn’t
>  consumed any input...Now that the (>>=) combinator immediately returns a Consumed
>  constructor as soon as some input has been consumed, the choice combinator
>  can choose an alternative as soon as some input has been consumed."

```haskell
{- If 'p' succeeds without consuming input, the second alternative is favored. 
   This implements the longest-match rule or maximal munch. -}
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  \input -> case p input of
    Empty Error -> q input
    Empty ok    -> case q input of
                     Empty _  -> Empty ok
                     consumed -> consumed
    consumed -> consumed
```
