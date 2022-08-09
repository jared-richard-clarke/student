;; monoid: a set equipped with an associative binary operation and an identity element

(define (monoid operation identity)
  (lambda operands
    (foldl (lambda (operand total)
             (operation total operand))
           identity
           operands)))

(define sum (monoid + 0))
(define product (monoid * 1))
