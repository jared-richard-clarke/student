# Concatenative Programming

- Concatenative Calculus ≈ Combinatory Logic + Continuation-Passing Style
- Variables are the `GOTO`s of concatenative programming.

## Lambda Calculus

```
e ::= x           ; Variables
  |   λx.e        ; Abstraction
  |   e1 e2       ; Application

λx.M[x] → λy.M[y] ; α-conversion

(λx.M)E → M[E/x]  ; β-reduction
```

## Turing Machines

```
M = (Q, Γ, b, ∑, δ, q₀, F)

Q                              ; Finite, non-empty set of states
Γ                              ; Finite, non-empty set of alphabet symbols
b ∈ Γ                          ; Blank symbol
∑ ⊆ Γ \ {b}                    ; Set of input symbols
q₀ ∈ Q, F ⊆ Q                  ; Initial and final states
δ                              ; State transition function
δ:(Q \ F) × Γ → Q × Γ × {L, R}

; Begin with initial state and tape.
; Repeat:
;    - If final state, then halt.
;    - Apply transition function.
;    - Modify tape.
;    - Move left or right.
```

## Combinatory Logic

The most basic combinators are `S` and `K`. `S` applies a function to an
argument within an environment. `K` selects 1 of 2 arguments.

```
Sxyz = xz(yz)          ; Application
S    = λx.λy.λz.xy(yz) ; "Starling"

Kxy  = x               ; Constant
K    = λx.λy.x         ; "Kestrel"

Ix   = x               ; Identity
I    = λx.x            ; "Idiot"

Bxyz = x(yz) ; Compose
Cxyz = xzy   ; Flip
Kxy = x      ; Constant
Wxy = xyy    ; Duplicate

SKKx = Kx(Kx) = x

M = SII = λx.xx
L = CBM = λf.λx.f(xx)
Y = SLL = λf.(λx.f(xx))(λx.f(xx))
```

## Concatenative Calculus

**The Theory of Concatenative Combinators** by Brent Kirby

```
; === Turing-complete combinators ===
E ::= C     ; Combinator
  |   [E]   ; Quotation
  |   E₁ E₂ ; Composition
            ; (E₂ ∘ E₁)

[A] dup      = [A] [A]
[A] [B] swap = [B] [A]
[A] drop     =
[A] quote    = [[A]]
[A] [B] cat  = [A B]
[A] call     = A
```

### Smaller Basis

```
[B] [A] k    = A
[B] [A] cake = [[B] A] [A [B]]

[B] [A] cons = [[B] A]
[B] [A] take = [A [B]]
```
