;; monoid: a set equipped with an associative binary operation and an identity element

;; (monoid function any) -> function
;; Intended as a factory function for producing monoid functions.
;; (define sum (monoid + 0)) -> (sum 1 2 3 4 5) -> 15

(define (monoid operation identity)
  (lambda operands
    (fold-left (lambda (total operand)
                 (operation total operand))
               identity
               operands)))
