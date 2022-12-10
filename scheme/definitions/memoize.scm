;; (memoize function) -> function
;; Wraps a function in a function that caches previously-computed values.
;; (define fib-cache (memoize fibonacci))

(define (memoize fn)
  (define cache '())
  (lambda xs
    (cond
      [(assq xs cache) => cdr]
      [else
       (let ([result (apply fn xs)])
         (set! cache (cons (cons xs result) cache))
         result)])))
