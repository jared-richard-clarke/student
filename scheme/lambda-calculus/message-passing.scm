;; Message passing style as defined in The Structure and Interpretation of Computer Programs.
;; Implementation of cons cell using procedures. Alternative implementation of Church Encoding.

(define (cons x y)
  (define (dispatch message)
    (cond [(= message 0) x]
          [(= message 1) y]
          [else (error "Argument not 0 or 1 -- CONS" message)]))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))
