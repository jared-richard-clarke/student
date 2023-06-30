# Compiling Haskell

**Source**: [A Haskell Compiler](https://www.scs.stanford.edu/11au-cs240h/notes/ghc.html) from Stanford University

## Why Haskell is Difficult

Haskell is seen as a difficult language to understand from a compilation perspective.

1. Higher order functions
2. Lazy evaluation
3. Partial application
4. Syntax that hides allocation
5. Typechecker

## Terminology

- **Values**: Both data and functions
- **Unboxed**: Primitive (machine level) types, not "boxed up" on the heap
- **Closure**: Heap allocated data associated with a method
- **Thunk**: A suspended computation
- **Continuations**: The cousins of closures. The continuation of a saved execution state. Can be built with tail calls and closures.

## Core

The main intermediate language used by GHC. Core is a variant of a **System FC**, which is itself a variant of **System F**

- variables 
- literals 
- let 
- case 
- lambda abstraction 
- application

```haskell
data Expr b -- "b" for the type of binders, 
  = Var    Id
  | Lit   Literal
  | App   (Expr b) (Arg b)
  | Lam   b (Expr b)
  | Let   (Bind b) (Expr b)
  | Case  (Expr b) b Type [Alt b]

  | Type  Type
  | Cast  (Expr b) Coercion
  | Coercion Coercion

  | Tick  (Tickish Id) (Expr b)

data Bind b 
  = NonRec b (Expr b)
  | Rec [(b, (Expr b))]

type Arg b = Expr b

type Alt b = (AltCon, [b], Expr b)

data AltCon 
  = DataAlt DataCon 
  | LitAlt  Literal 
  | DEFAULT
```

## Graph Reduction

### Graph Reduction Modell

- **redex(es)**: reducible expression. A expression that can be evaluated further.
  - **normal form**: an expression without any redexes.
  - **head normal form**: an expression where the top level (head) is neither a redex nor a lambda abstraction with a reducible body.
  - **weak head normal form**: an expression where the top level (head) isn't a redex.
- **unfolding**: unfolding of a function f is just the body of f.
  - Unfolding = Inlining.
- Graph reduction allows lazy evaluation and sharing
- **let**: adds new node to graph
- **case**: expression evaluation, causes the graph to be reduced
- When a node is reduced, it is replaced (or updated) with its result

### Evaluation Strategies

- **call-by-value**: arguments evaluated before function entered (copied)
- **call-by-name**: arguments passed unevaluated
- **call-by-need**: arguments passed unevaluated but an expression is only evaluated once (sharing)
- **no-strict evaluation** vs. **lazy evaluation**:
  - **non-strict**: Includes both call-by-name and call-by-need, general term for evaluation strategies that don't evaluate arguments before entering a function.
  - **lazy evaluation**: Specific type of non-strict evaluation. Uses call-by-need (for sharing).

## Functions -> Core

```haskell
-- Haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

-- Haskell Core
map :: forall a b. (a -> b) -> [a] -> [b]
map =
  \ (@ a) (@ b) (f :: a -> b) (xs :: [a]) ->
    case xs of _ {
      []     -> GHC.Types.[] @ b;
      : y ys -> GHC.Types.: @ b (f y) (map @ a @ b f ys)
    }
```

## `data` -> Core

```haskell
-- Haskell
data Maybe a = Nothing | Just a

none = Nothing
some = Just (1 :: Int)

-- Haskell Core
none :: forall a. Maybe a
none = Nothing

n :: GHC.Types.Int
n = GHC.Types.I# 1

some :: Maybe GHC.Types.Int
some = Just @ GHC.Types.Int n
```

## `where` -> Core

```haskell
-- Haskell
dox :: Int -> Int
dox n = x * x
    where x = (n + 2) * 4
    
 -- Haskell Core
dox :: GHC.Types.Int -> GHC.Types.Int
dox =
  \ (n :: GHC.Types.Int) ->
    let {
      x :: GHC.Types.Int
      x =
        GHC.Num.* @ GHC.Types.Int GHC.Num.$fNumInt
          (GHC.Num.+ @ GHC.Types.Int GHC.Num.$fNumInt n (GHC.Types.I# 2))
          (GHC.Types.I# 4) }

    in GHC.Num.* @ GHC.Types.Int GHC.Num.$fNumInt x x
```

## Core Summary

- Pattern matching and guards are translated to `case` expressions
- `where` expressions become `let` expressions
- language still lazy but looking for `let` and `case` gives you a good idea of evaluation order
- `case` means evaluation. (e.g `seq` is translated to case)
- `let` expressions are allocation of closures
- function application is a thunk
- operations involving unboxed types are eager
