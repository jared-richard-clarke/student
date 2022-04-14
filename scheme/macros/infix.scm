;; (infix expression) -> number | error
;; Evaluates infix expression implemented as a list.
;; (infix '(2 × 5)) -> 10
(define (infix expr)
  (let ([a (car expr)]
        [op (cadr expr)]
        [b (caddr expr)])
    (case op
      [(+)     ((lambda (x y) (+ x y)) a b)]
      [(-)     ((lambda (x y) (- x y)) a b)]
      [(* ⋅ ×) ((lambda (x y) (* x y)) a b)]
      [(/ ÷)   ((lambda (x y) (/ x y)) a b)]
      [else (error "invalid operator:" op)])))

;; === macro expansion ===
(define infix-ex
  (lambda (expr)
    ((lambda (a op b)
       (if (member op (quote (+)))
           (begin ((lambda (x y) (+ x y)) a b))
           (if (member op (quote (-)))
               (begin ((lambda (x y) (- x y)) a b))
               (if (member op (quote (* ⋅ ×)))
                   (begin ((lambda (x y) (* x y)) a b))
                   (if (member op (quote (/ ÷)))
                       (begin ((lambda (x y) (/ x y)) a b))
                       (begin (error "invalid operator:" op)))))))
     (car expr)
     (cadr expr)
     (caddr expr))))
