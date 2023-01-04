# `when` macro

## Scheme

```scheme
(define-syntax when
  (lambda (stx)
    (syntax-case stx ()
      [(when test x y ...)
       (syntax (if test
                   (begin x y ...)
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
