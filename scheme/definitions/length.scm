;; (length list) -> number
;; Counts elements in proper list.
;; (length '(a b c)) -> 3
;; (length '()) -> 0

(define length
  (lambda (xs)
    (fold-left (lambda (x accum) (+ accum 1)) 0 xs)))
