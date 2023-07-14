# Church Encoding of Pairs

## The Lambda Calculus

Pairs are represented as a higher-order functions that take a function argument. Application of
the function argument returns one of the elements of the pair contained within the closure
of the higher-order function.

```
pair   ≡ λx.λy.λz.z x y
first  ≡ λp.p(λx.λy.x)
second ≡ λp.p(λx.λy.y)
```

## Scheme Implementation

```scheme
(define pair
  (lambda (x y)
    (lambda (message) (message x y))))

(define first
  (lambda (call)
    (call (lambda (x y) x))))

(define second
  (lambda (call)
    (call (lambda (x y) y))))
```
