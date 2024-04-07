;; Flags
(define SOME 'SOME)
(define NONE 'NONE)

;; Wraps a value that can be something or nothing.
;; The flag 'SOME denotes the presence of a value.
;; The flag 'NONE denotes the absence of a value.
(define-record-type option
  (fields flag    ;; SOME | NONE
          value)) ;; any

;; (vector-iter vector) -> (function) -> struct option
;; Wraps a vector in a closure, returning an element of
;; the vector for each function call until the vector is
;; exhausted, meaning the last element in the vector has
;; been reached.
;; Every returned value is wrapped in an "option" struct.
;; Every "option" with the flag 'SOME contains an element
;; of the vector. An "option" with the flag 'NONE means
;; the vector has been exhausted.
(define vector-iter
  (lambda (xs)
    (let ([index -1]
          [step 1]
          [limit (- (vector-length xs) 1)])
      (lambda ()
        (if (< index limit)
            (begin
              (set! index (+ index step))
              (make-option SOME (vector-ref xs index)))
            (make-option NONE '()))))))
