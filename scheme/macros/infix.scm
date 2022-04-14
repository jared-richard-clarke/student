;; === case syntax ===
(define (infix expr)
  (let ([op (cadr expr)]
        [a (car expr)]
        [b (caddr expr)])
    (case op
      [(+)     ((lambda (x y) (+ x y)) a b)]
      [(-)     ((lambda (x y) (- x y)) a b)]
      [(* ⋅ ×) ((lambda (x y) (* x y)) a b)]
      [(/ ÷)   ((lambda (x y) (/ x y)) a b)]
      [else (error "invalid operator:" op)])))

;; === case-syntax expansion ===
(define (infix-ex expr)
  (let ([op (cadr expr)]
        [a (car expr)]
        [b (caddr expr)])
    (if (member op '(+))
        (begin ((lambda (x y) (+ x y)) a b))
        (if (member op '(-))
            (begin ((lambda (x y) (- x y)) a b))
            (if (member op '(* ⋅ ×))
                (begin ((lambda (x y) (* x y)) a b))
                (if (member op '(/ ÷))
                    (begin ((lambda (x y) (/ x y)) a b))
                    (begin (error "invalid operator:" op))))))))
