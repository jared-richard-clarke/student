# Consumer-Based Parsing

**Parsec: Direct Style Parser Combinators For The Real World** by Daan Leijen and Erik Meijer

## Basic Combinators

```haskell
type Parser a = String -> Consumed a

data Consumed a = Consumed (Reply a) | Empty (Reply a)

data Reply a = Ok a String | Error

return x input = Empty (Ok x input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy test input = 
    case input of
        [] -> Empty Error
        (c:cs) | test c    -> Consumed (Ok c cs)
               | otherwise -> Empty Error
```

## `>>=`: input consumption

> "Due to laziness, a parser `p >>= f` directly returns with a `Consumed` constructor
>  if `p` consumes input. The computation of the final `Reply` is delayed."
>  
>  — Daan Leijen and Erik Meijer

```haskell
(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f =
    \input -> case p input of
        Empty reply1
            -> case reply1 of
                   Ok x rest -> (f x) rest
                   Error     -> Empty Error
        Consumed reply1
            -> Consumed
               (case reply1 of
                    Ok x rest
                          -> case (f x) rest of
                                 Consumed reply2 -> reply2
                                 Empty reply2    -> reply2
                    error -> error)
```

| `p`        | `q`        | `p >>= q`  |
| ---------- | ---------- | ---------- |
| `Empty`    | `Empty`    | `Empty`    |
| `Empty`    | `Consumed` | `Consumed` |
| `Consumed` | `Empty`    | `Consumed` |
| `Consumed` | `Consumed` | `Consumed` |

## Choice

> "An LL(1) choice combinator only looks at its second alternative if the first hasn’t
>  consumed any input...Now that the `>>=` combinator immediately returns a `Consumed`
>  constructor as soon as some input has been consumed, the choice combinator
>  can choose an alternative as soon as some input has been consumed."
>  
>  — Daan Leijen and Erik Meijer

```haskell
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
    \input -> case p input of
        Empty Error -> q input
        Empty ok    -> case q input of
                           Empty _  -> Empty ok
                           consumed -> consumed
        consumed    -> consumed
```

## Try

> "For `try p <|> q`, if parser `p` fails, the choice operator will try the alternative `q`
>  since the `try` combinator has changed the `Consumed` constructor to `Empty`."
>  
>  — Daan Leijen and Erik Meijer

```haskell
try :: Parser a -> Parser a
try p input =
    case p input of
        Consumed Error -> Empty Error
        other          -> other
```

## Parsing with Error Handling

An error message contains a position, the unexpected input, and a list of
expected productions – the first set.

### Output on Error

```
> run identifier ""
  parse error at (line 1, column 1):
  unexpected end of input
  expecting letter, digit, or _
```

### Code

```haskell
type Parser a = State -> Consumed a

data State = State String Pos

-- Position, unexpected input, and expected productions.
data Message = Message Pos String [String]

data Reply a = Ok a State Message | Error Message

-- The "return" parser attaches an empty message to the parser reply.

return :: a -> Parser a
return x state = Empty (Ok x state (Message pos [] []))

{-
  "satisfy" updates the parser position if successful and returns an error
  message with the current position and input if it fails.
-}
satisfy :: (Char -> Bool) -> Parser Char
satisfy test =
    \(State input pos) ->
        case input of
            (c:cs) | test c
                   -> let newPos   = nextPos pos c
                          newState = State cs newPos
                      in seq newPos
                             (Consumed
                                 (Ok c newState
                                     (Msg pos [] [])))
            (c:cs) -> Empty (Error
                          (Msg pos [c] []))
            []     -> Empty (Error
                          (Msg pos "end of input" []))

{-
  "<|>" computes the dynamic first set by merging the error messages of two "Empty"
  alternatives, regardless of their reply value. Whenever both alternatives do not
  consume input, both of them contribute to the possible causes of failure. Even
  when the second succeeds, the first alternative should propagate its error messages
  into the "Ok" reply.
-}
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q =
    \state -> 
        case p state of
            Empty (Error msg1)
                      -> case q state of
                             Empty (Error msg2)
                                 -> mergeError msg1 msg2
                             Empty (Ok x inp msg2)
                                 -> mergeOk x inp msg1 msg2
                             consumed
                                 -> consumed
            Empty (Ok x inp msg1)
                      -> case q state of
                             Empty (Error msg2)
                                 -> mergeOk x inp msg1 msg2
                             Empty (Ok _ _ msg2)
                                 -> mergeOk x inp msg1 msg2
                             consumed
                                 -> consumed
            consumed -> consumed

mergeOk x inp msg1 msg2 =
    Empty (Ok x inp (merge msg1 msg2))

mergeError msg1 msg2 =
    Empty (Error (merge msg1 msg2))

merge (Msg pos inp exp1) (Msg _ _ exp2) =
    Msg pos inp (exp1 ++ exp2)

{-
  The parser "p <?> msg" behaves like parser "p" but when it fails *without* consuming
  input, it sets the expected productions to "msg". The label combinator is used to
  return error messages in terms of high-level grammar productions rather than at
  a character level.
-}
(<?>) :: Parser a -> String -> Parser a
p <?> exp =
    \state ->
        case p state of
            Empty (Error msg)
                  -> Empty (Error (expect msg exp))
            Empty (Ok x st msg)
                  -> Empty (Ok x st (expect msg exp))
            other -> other

expect (Msg pos inp _) exp =
    Msg pos inp [exp]
```
