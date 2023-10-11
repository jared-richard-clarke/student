# Lambda Encodings

## Church Numerals

```
0 = λa.λb.b
1 = λa.λb.ab
2 = λa.λb.a(ab)
3 = λa.λb.a(a(ab)) ...
```

## Control Flow

```
True  ≡ λx.(λy.x)
False ≡ λx.(λy.y)

and = λp.λq.pqp
or  = λp.λq. ppq
not = λp.λa.λb.pba
if  = λp.λa.λb.pab
```

## Recursion

```
Y  ≡  λf.(λx.f(xx))(λx.f(xx))
```

## Currying

```
(A × B) -> C

-isomorphic->

A -> (B -> C)
```

## Pair (cons cell)

### Lambda Calculus

```
pair  ≡ λx.λy.λf.fxy
head  ≡ λp.p (λx.λy.x)
tail  ≡ λp.p (λx.λy.y)
```

### Scheme

```scheme
(define pair
  (lambda (x y)
    (lambda (f) (f x y))))

(define head
  (lambda (p)
    (p (lambda (x y) x))))

(define tail
  (lambda (p)
    (p (lambda (x y) y))))
```
