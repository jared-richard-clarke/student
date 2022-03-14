;; (memoize function) -> function -> value
;; Wraps function in a function that stores previously-computed values.
;; Eliminates redundant computation.
;; (define add (memoize (lambda (x y) (+ x y))))
;; (add 1 6) -> caches then returns 7

(define (memoize function)
  (define cache '())
  (lambda args
    (let ([cached (assoc args cache)])
      (if (not cached)
          (let ([result (apply function args)])
            (set! cache (cons (cons args result) cache))
            result)
          (cdr cached)))))
