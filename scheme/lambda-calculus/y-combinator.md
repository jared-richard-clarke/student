# The Y Combinator

Achieves recursion without named functions.

## Lambda Calculus

```
Y = λf.(λx.f (x x)) (λx.f (x x))
```

## Scheme

Implementated in Scheme as defined in The Little Schemer.

```scheme
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
```

## JavaScript

Implemented in JavaScript as defined by Douglas Crockford.

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
