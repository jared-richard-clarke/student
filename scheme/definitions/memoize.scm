;; (memoize function) -> (function any ...) -> any
;; Wraps a function in a function that caches values so that they need not be recomputed.
;; Maps a tuple of inputs to its output within an associative list.
;; (define fib-cache (memoize fibonacci))

(define (memoize fn)
  (define cache '())
  (lambda xs
    (cond
      [(assoc xs cache) => cdr]
      [else
       (let ([result (apply fn xs)])
         (set! cache (cons (cons xs result) cache))
         result)])))
