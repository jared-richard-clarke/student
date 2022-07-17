# Combinators

[Thinking Functionally](https://swlaschin.gitbooks.io/fsharpforfunandprofit/content/posts/defining-functions.html)
by Scott Wlaschin

Combinators are functions whose result depends only on their parameters.
They are the basis for combinatory logic

## Combinator Functions

Combinator functions defined as operators.

```fsharp
let (|>) x f = f x             // forward pipe
let (<|) f x = f x             // reverse pipe
let (>>) f g x = g (f x)       // forward composition
let (<<) g f x = g (f x)       // reverse composition
```

## Standard Combinators

In *To Mock a Mockingbird* by Raymond Smullyan, combinators are named after birds. 
Any computable function can be implemented using only the *Kestral* and the *Starling*.

```fsharp
let I x = x                // identity function, or the Idiot bird
let K x y = x              // the Kestrel
let M x = x >> x           // the Mockingbird
let T x y = y x            // the Thrush (pipe operation)
let Q x y z = y (x z)      // the Queer bird (forward composition)
let S x y z = x z (y z)    // The Starling
// and the infamous...
let rec Y f x = f (Y f) x  // Y-combinator, or Sage bird
```
