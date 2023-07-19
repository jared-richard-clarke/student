# Church Encoding of Pairs

## The Lambda Calculus

```
pair  ≡ λx.λy.λf.f x y
head  ≡ λf.f (λx.λy.x)
tail  ≡ λf.f (λx.λy.y)

   head (pair p q)
 = (λf.f (λx.λy.x)) ((λx.λy.λf. f x y) p q)
-> (λx.λy.λf. f x y) p q (λx.λy.x)
-> (λy.λf.f p y) q (λx.λy.x)
-> (λf.f p q) (λx.λy.x)
-> (λx.λy.x) p q
-> (λy.p) q
-> p
```

## Scheme Implementation

```scheme
(define pair
  (lambda (x y)
    (lambda (f) (f x y))))

(define head
  (lambda (pair)
    (pair (lambda (x y) x))))

(define tail
  (lambda (pair)
    (pair (lambda (x y) y))))
```
