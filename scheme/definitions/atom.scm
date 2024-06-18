;; (atom? any) -> boolean
;; If argument is not a pair, returns #t.
;; Otherwise returns #f.

(define atom?
  (lambda (x)
    (not (pair? x))))
