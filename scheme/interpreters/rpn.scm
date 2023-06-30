;; Reverse Polish Notation calculator.
;; (rpn '(2 3 × 1 + -)) -> '(-7)

(define rpn
  (let ([stack '()]
        [environment (list (cons '+ +)
                           (cons '- -)
                           (cons '* *)
                           (cons '× *)
                           (cons '/ /)
                           (cons '÷ /))]
        [lookup (lambda (var env)
                  (let ([result (assq var env)])
                    (if result
                        (cdr result)
                        (error "undefined operator:" var))))])
    (lambda (expression)
      (fold-left (lambda (x total)
                   (if (number? x)
                       (cons x total)
                       (cons (apply (lookup x environment)
                                    (reverse total))
                             stack)))
                 stack
                 expression))))
