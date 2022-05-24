;; (not obj) -> boolean
;; Returns #t if value is false. Returns #f otherwise.
;; (not #f) -> #t

(define (not value)
  (if value #f #t))
