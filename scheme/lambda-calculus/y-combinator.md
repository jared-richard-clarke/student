# The Y or Fixed Point Combinator

```
fix f = f (fix f) -> f(f(...f (fix f)...))

Y F = fixed-point
Y F = F fixed-point

Y F = F (Y F) -> F (F (...F (Y F)...))
```

## Lambda Calculus

```
Y = λf.(λx.f (x x)) (λx.f (x x))
```

## Closed Factorial Function

Recursively calls itself until it reaches its fixed point of `n <= 2`. Similar to the `Y` combinator
but not as general.

```scheme
(define recur
  (lambda (f)
    (lambda (n)
      (if (<= n 2)
          n
          (* n ((f f) (- n 1)))))))

(define factorial (recur recur))
```

## Scheme

Y Combinator implemented in **The Little Schemer**.

```scheme
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; This definition of Y causes an infinite loop in a strictly-evaluated
;; language like Scheme.

(define Y
  (lambda (f)
    (f (Y f))))

;; Scheme is a strict language so the evaluation of `(f (Y f))`
;; must be delayed by wrapping it in a function — also called a thunk.
;; This, however, is not a true combinator. `Y` is a free variable
;; within its own definition.

(define Y
  (lambda (f)
    (lambda (x)
      ((f (Y f)) x))))
```
