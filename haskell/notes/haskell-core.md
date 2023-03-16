# Haskell Core

[Haskell to Core](https://serokell.io/blog/haskell-to-core)
by Vladislav Zavialov

> "...GHC’s Core: a small, elegant language, used as an intermediate
>  representation in GHC’s compilation pipeline. The many features of
>  Haskell are reducible to the few constructs of Core."
>
> — Vladislav Zavialov

## GHC Pipeline

1. Text
2. Lexical Analysis   -> Tokens
3. Syntactic Analysis -> Syntax Tree
4. Name Resolution    -> Well-Scoped Syntax Tree
5. Type Checking      -> Well-Typed Syntax Tree
6. Desugaring         -> Core
7. Stg ... Cmm ...

## Haskell Core

The Haskell core as presented by Vladislav Zavialov on 11 August 2020.

Haskell expression syntax has nine constructs.

1. Variables (`Var`)
2. Literal (`Lit`)
3. Function Application (`App`)
4. Lambdas (`Lam`)
5. Let Bindings (`Let`)
6. Case Expressions (`Case`)
7. Casts (`Cast`)
8. Coercions (`Coercion`)

```haskell
data Expr
  = Var      Id
  | Lit      Literal
  | App      Expr Expr
  | Lam      Var Expr
  | Let      Bind Expr
  | Case     Expr Var Type [Alt]
  | Cast     Expr Coercion
  | Type     Type
  | Coercion Coercion
  | Tick  ...  -- unimportant

type Alt = (AltCon, [Var], Expr)

data AltCon
  = DataAlt DataCon
  | LitAlt  Literal
  | DEFAULT

data Bind
  = NonRec Var Expr
  | Rec [(Var, Expr)]

data Type
  = TyVarTy Var
  | AppTy Type Type
  | TyConApp TyCon [Type]
  | ForAllTy TyCoVarBinder Type
  | FunTy Mult Type Type
  | LitTy TyLit
  | CastTy Type Coercion
  | CoercionTy Coercion
```

## Infix Operators

```haskell
-- Haskell
a && b

-- Core
(&&) a b

-- Core with Types
((&&) :: Bool -> Bool -> Bool)
   (a :: Bool)
   (b :: Bool)
```

## Function Bindings

```haskell
-- Haskell
f :: Integer -> Integer
f x = x

-- Core
f = \(x :: Integer) -> (x :: Integer)
```

### Multi-Argument Function Bindings

```haskell
-- Haskell
f x y = not x && y
-- or ->
f = \x y -> not x && y

-- Core
f = \(x :: Bool) -> \(y :: Bool) -> (&&) (not x) y
```

## Pattern Bindings

```haskell
-- Haskell
a :: Integer
b :: Bool
(a, b) = (1, True)

-- Core
ab = (1, True)

a = case ab of
    (,) x y -> x

b = case ab of
    (,) x y -> y
```

## Operator Sections

```haskell
-- Haskell
(a &&)
-- and ->
(&& b)

-- Core
\b -> (&&) a b
-- and ->
\a -> (&&) a b
```

## Tuple Sections

```haskell
-- Haskell
(,True,)

-- Core
\a -> \b -> (,,) a True b
```

## Multi-Argument Pattern Matching

```haskell
-- Haskell
and True  True  = True
and True  False = False
and False True  = False
and False False = False

-- Core
and =
  \(a :: Bool) ->
  \(b :: Bool) ->
  case a of
    False ->
      case b of
        False -> False
	True  -> False
    True ->
      case b of
        False -> False
	True  -> True
```

## Deep Pattern Matching

```haskell
-- Haskell
f (Left (Just "")) = True
f (Right ()) = True
f _ = False

-- Core
f =
  \e ->
    case e of
      Left l ->
        case l of
	  Nothing -> False
	  Just s  ->
	    case s of
	      []      -> True
	      (:) a b -> False
      Right r ->
        case r of
	  () -> True
```

## If-Then-Else

```haskell
-- Haskell
if c then a else b

-- Core
case c of
  True  -> a
  False -> b
```

## `seq`

> "The `seq` function, which forces evaluation of its argument to
>  weak-head normal form, is desugared into a case-expression,
>  relying on the fact that in Core, case-expressions are strict"
>
> — Vladislav Zavialov

```haskell
-- Haskell
seq a b

-- Core
case a of
  _ -> b
```

### Bang (`!`) Patterns

```haskell
f, g :: Bool -> Bool
f x  = x
g !x = x

-- Core
f \x -> x
g = \x -> case x of
            _ -> x
```

## Parametric Polymorphism

```haskell
-- Haskell
id :: forall a. a -> a
id x = x
-- and ->
t :: Bool
t = id True

-- Core
id = \ @(a :: Type) ->
     \  (x :: a) ->
         x
-- and ->
t = id @Bool True

-- Haskell
p = (True, 'x', "Hello")

-- Core
p = (,,) @Bool @Char @String True 'x' "Hello"
```

## Classes and Dictionary Passing

> "In Core, there are no type classes. Class instances are passed
>  around in Core explicitly, as ordinary data. These values are
>  called 'dictionaries'..."
>
> — Vladislav Zavialov

```haskell
-- Haskell
f :: Num a => a -> a
f x = x + x

-- Core
f = \ @(a :: Type) ->
    \ ($dNum :: Num a) ->
    \ (x :: a) ->
      (+) @a $dNum x x

{-
  1. `a :: Type` -> The type of input/output.
  2. `$dNum :: Num a` -> The class dictionary with methods.
  3. `x :: a` -> The input value.
-}

-- Haskell
f :: Int -> Int
f x = x + x

-- Core
f = \(x :: Int) -> (+) @Int $dNumInt x x
```

## Do Notation

```haskell
-- Haskell
f act = do
  x <- act
  y <- act
  return (x && y)

-- Not Quite Core
f = \act ->
  act >>= \x ->
  act >>= \y ->
  return (x && y)

-- Core
f = \ @(m :: Type -> Type) ->
    \  ($dMonad :: Monad m) ->
    \  (act :: m Bool) ->
      (>>=)  @m $dMonad @Bool @Bool act (\x ->
      (>>=)  @m $dMonad @Bool @Bool act (\y ->
      return @m $dMonad @Bool ((&&) x y)))
```
