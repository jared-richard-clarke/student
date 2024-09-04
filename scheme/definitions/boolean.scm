;; (boolean? any) -> boolean
;; Is object boolean "#t" or "#f"?
;; (boolean? #f) -> #t
(define boolean?
  (lambda (x)
    (or (eq? x #t) (eq? x #f))))
