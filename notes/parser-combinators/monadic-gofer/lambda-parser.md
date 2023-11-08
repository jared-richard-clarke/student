# A Parser for λ-expressions

**Monadic Parser Combinators** by Graham Hutton and Erik Meijer,
chapter 6.2, page 24

```haskell
data Expression = Apply Expression Expression      -- application
                | Lambda String Expression         -- lambda abstraction
                | Let String Expression Expression -- local definition
                | Variable String                  -- variable

expression = atom `chainl1` [Apply]                -- (f x y z) - parsed as -> (((f x) y) z)

atom = lambda +++ local +++ name +++ parentheses

lambda = [Lambda x e | _ <- symbol "\\"
                     , x <- name
                     , _ <- symbol "->"
                     , e <- expression]
                
local = [Let x e e' | _  <- symbol "let"
                    , x  <- name
                    , _  <- symbol "="
                    , e  <- expression
                    , _  <- symbol "in"
                    , e' <- expression]
                    
variable = [Variable x | x <- name]

parentheses = bracket (symbol "(") expression (symbol ")")

name = identifier ["let","in"]
```
