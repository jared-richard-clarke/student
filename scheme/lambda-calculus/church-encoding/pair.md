# Church Encoding of Pairs

## The Lambda Calculus

```
pair   ≡ λx.λy.λf.f x y
first  ≡ λp.p(λx.λy.x)
second ≡ λp.p(λx.λy.y)
```

## Scheme Implementation

```scheme
(define pair
  (lambda (x y)
    (lambda (f) (f x y))))

(define first
  (lambda (pair)
    (pair (lambda (x y) x))))

(define second
  (lambda (pair)
    (pair (lambda (x y) y))))
```
