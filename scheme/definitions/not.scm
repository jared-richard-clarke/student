;; (not any) -> boolean
;; Outputs #t if input evaluates false. Returns #f otherwise.
;; Follows Scheme implementation for evaluation of non-boolean expressions.
;; (not #f) -> #t

(define (not x)
  (if x #f #t))
