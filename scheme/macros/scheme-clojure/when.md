# `when` macro

## Scheme

```scheme
(define-syntax when
  (lambda (stx)
    (syntax-case stx ()
      [(_ x y z ...)
       (syntax (if x
                   (begin y z ...)
                   (void)))])))
```

## Clojure

```clojure
(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))
```
