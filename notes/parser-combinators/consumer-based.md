# Direct Style Monadic Parsers

## Consumer-Based Approach

```haskell
{-
  "To use an LL(1) strategy, each parser keeps track of its input consumption.
   We call this the consumer-based approach. A parser has either Consumed
   input or returned a value without consuming input, Empty. The return value
   is either a single result and the remaining input, Ok a String, or a parse
   error, Error.
-}


type Parser a   = String -> Consumed a

data Consumed a = Consumed (Reply a) | Empty (Reply a)

data Reply a    = Ok a String | Error

return x = \input -> Empty (Ok x input)

satisfy :: (Char -> Bool) -> Parser Char
satisfy test = \input -> case (input) of
                 []     -> Empty Error
		 (c:cs) | test c    -> Consumed (Ok c cs)
		        | otherwise -> Empty Error

char c = satisfy (==c)
letter = satisfy isAlpha
digit  = satisfy isDigit

{-
  "Due to laziness, a parser (p >>= f) directly returns with a Consumed
   constructor if p consumes input. The computation of the final reply
   value is delayed. This 'early' returning is essential for the efficient
   behavior of the choice combinator."
-}

p >>= f = \input -> case (p input) of
            Empty reply1
	      -> case (reply1) of
	           Ok x rest -> ((f x) rest)
		   Error     -> Empty Error

            Consumed reply1
	      -> Consumed (case (reply1) of
	                     Ok x rest
			            -> case ((f x) rest) of
				         Consumed reply2 -> reply2
				    Empty reply2 -> reply2
			     error -> error
			   )

p <|> q = \input -> case (p input) of
            Empty Error -> (q input)
	    Empty ok    -> case (q input) of
	                     Empty _  -> Empty ok
			     consumed -> consumed
	    consumed    -> consumed

string :: String -> Parser ()
string ""     = return ()
string (c:cs) = do{ char c; string cs }

{-
  "...many1 parser works because the choice combinator doesn't backtrack."
-}

many1 :: Parser a -> Parser [a]
many1 p = do { x  <- p
             ; xs <- (many1 p <|> return [])
	     ; return (x:xs)
	     }

identifier = many1 (letter <|> digit <|> char '_')
```
