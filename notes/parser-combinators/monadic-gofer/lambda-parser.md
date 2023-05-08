# A Parser for λ-expressions

**Monadic Parser Combinators** by Graham Hutton and Erik Meijer,
chapter 6.2, page 24

```haskell
data Expr = App Expr Expr        -- application
          | Lam String Expr      -- lambda abstraction
          | Let String Expr Expr -- local definition
          | Var String           -- variable

expr = atom `chainl1` [App]      -- (f x y z) - parsed as -> (((f x) y) z)

atom = lam +++ local +++ var +++ paren

lam = [Lam x e | _ <- symbol "\\"
               , x <- variable
               , _ <- symbol "->"
               , e <- expr]
               
local = [Let x e e' | _  <- symbol "let"
                    , x  <- variable
                    , _  <- symbol "="
                    , e  <- expr
                    , _  <- symbol "in"
                    , e' <- expr]
                    
var = [Var x | x <- variable]

paren = bracket (symbol "(") expr (symbol ")")

variable = identifier ["let","in"]
```
