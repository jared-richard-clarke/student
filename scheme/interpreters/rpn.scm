;; Reverse Polish Notation Calculator.
;; (rpn '(2 3 × 1 + -)) -> -7
;;
;; Side Note: Unlike true RPN, operators have variable arity, meaning
;; (rpn '(1 2 3 4 5 6 7 8 9 10 +)) will evaluate to 55.

(define rpn
  ;; === calculator environment ===
  (let ([stack '()]
        [table (list (cons '+ +)
                     (cons '- -)
                     (cons '* *)
                     (cons '× *)
                     (cons '/ /)
                     (cons '÷ /)
                     (cons '= =))]
        [lookup (lambda (var env)
                  (let ([result (assq var env)])
                    (if result
                        (cdr result)
                        (error "undefined operator:" var))))]
        [return (lambda (stack)
                  (if (= (length stack) 1)
                      (car stack)
                      (error "incomplete expression:" (reverse stack))))])
    ;; === calculator function ===
    (lambda (expression)
      (return (fold-left (lambda (x total)
                           (if (number? x)
                               (cons x total)
                               (cons (apply (lookup x table)
                                            (reverse total))
                                     stack)))
                         stack
                         expression)))))
