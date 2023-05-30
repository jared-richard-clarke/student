;; (not any) -> boolean
;; Outputs #t if input evaluates false. Returns #f otherwise.
;; Follows Scheme implementation for evaluation of non-boolean expressions.
;; Generally, any expression that is not #f is equivalent to #t.
;; (not #f) -> #t
;; (not 7)  -> #f

(define (not x)
  (if x #f #t))
