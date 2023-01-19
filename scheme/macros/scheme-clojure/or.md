# `or` macro

## Scheme

```scheme
(define-syntax or
  (lambda (stx)
    (syntax-case stx ()
      [(_)   (syntax #f)]
      [(_ x) (syntax y)]
      [(_ x y z ...)
       (syntax (let ([t x])
                 (if t t (or y z ...))))])))
```

## Clojure

```clojure
(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))
```
