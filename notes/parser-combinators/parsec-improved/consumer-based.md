# Consumer-Based Parsing

Copied from *Parsec: Direct Style Monadic Parser Combinators For The Real World*
by Daan Leijen and Erik Meijer

## Input Consumption of `(>>=)`

| `p`        | `q`        | `(p >>= q)` |
| ---------- | ---------- | ----------- |
| `Empty`    | `Empty`    | `Empty`     |
| `Empty`    | `Consumed` | `Consumed`  |
| `Consumed` | `Empty`    | `Consumed`  |
| `Consumed` | `Consumed` | `Consumed`  |

## Parser

```haskell
{- Module, exports, and imports omitted -}
type Parser a = String -> Consumed a
data Consumed a = Consumed (Reply a) | Empty (Reply a)
data Reply a = Ok a String | Error
return :: a -> String -> Consumed a
return x = \input -> Empty (Ok x input)
satisfy :: (Char -> Bool) -> Parser Char
satisfy test = \input -> case input of
  [] -> Empty Error
  (c : cs)
    | test c -> Consumed (Ok c cs)
    | otherwise -> Empty Error
char :: Char -> Parser Char
char c = satisfy (== c)
letter :: Parser Char
letter = satisfy isAlpha
digit :: Parser Char
digit = satisfy isDigit
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f =
  \input -> case p input of
    Empty reply1 -> case reply1 of
      Ok x rest -> f x rest
      Error -> Empty Error
    Consumed reply1 ->
      Consumed
        ( case reply1 of
            Ok x rest ->
              case f x rest of
                Consumed reply2 -> reply2
                Empty reply2 -> reply2
            Error -> Error
        )
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
  \input -> case p input of
    Empty Error -> q input
    Empty ok -> case q input of
      Empty _ -> Empty ok
      consumed -> consumed
    consumed -> consumed
```
