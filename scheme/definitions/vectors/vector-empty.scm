;; (vector-empty? vector) -> boolean
;; Checks whether a vector is empty.
;; (vector-empty? '#()) -> #t
;; (vector-empty? '#(1 2 3)) -> #f

(define vector-empty?
  (lambda (v)
    (zero? (vector-length v))))
