# The Y or Fixed Point Combinator

Achieves recursion without named functions.

## General Outline

```
fixpoint = f(fixpoint)
fixpoint = f(f(fixpoint))
...

Y(f) = fixpoint
Y(f) = f(fixpoint)

Y(f) = f(Y(f))
```

## Closed Factorial Function

Recursively calls itself until it reaches its fixed point of `n <= 2`. Similar to the `Y` combinator
but not as general.

```javascript
function recur(f) {
    return function(n) {
        return n <= 2 ? n : n * f(f)(n - 1);
    }
}

const factorial = recur(recur);
```

## Lambda Calculus

```
Y = λf.(λx.f (x x)) (λx.f (x x))
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

## JavaScript

Y Combinator implemented by Douglas Crockford.

```javascript
function Y(le) {
    return (function (f) {
        return f(f);
    })(function (f) {
        return le(function (x) {
            return f(f)(x);
        });
    });
}

const factorial = Y(function (fac) {
    return function (n) {
        return n <= 2 ? n : n * fac(n - 1);
    };
});
```

## OCaml

```ocaml
let rec Y f x = f (Y f) x
```
