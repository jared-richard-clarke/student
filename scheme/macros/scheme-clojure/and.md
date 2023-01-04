# `and` macro

## Scheme

```scheme
(define-syntax and
  (lambda (stx)
    (syntax-case stx ()
      [(_) (syntax #t)]
      [(_ x) (syntax x)]
      [(_ x y ...)
       (syntax (if x (and y ...) #f))])))
```

## Clojure

```clojure
(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))
```
