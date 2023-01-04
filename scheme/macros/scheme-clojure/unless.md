# `unless` macro

## Scheme

```scheme
(define-syntax unless
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y z ...)
       (syntax (if (not x)
                   (begin y z ...)
                   (void)))])))
```

## Clojure

```clojure
(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
    (list 'if test nil (cons 'do body)))
```
