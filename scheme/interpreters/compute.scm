;; (compute list) -> number
;; Evaluates arithmetic expressions in Scheme.
;; (compute '(+ 1 2 3 4 5 6 7 8 9 10)) -> 55

(define env
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '× *)
        (cons '/ /)
        (cons '÷ /)
        (cons '^ expt)
        (cons '√ sqrt)
        (cons 'π 3.141592653589793)
        (cons 'e 2.718281828459045)
        (cons 'φ 1.618033988749894)))

(define (lookup var env)
  (let ([result (assq var env)])
    (if result
        (cdr result)
        (error "undefined operator: " var))))

(define (compute expr)
  (cond
    [(symbol? expr) (lookup expr env)]
    [(number? expr) expr]
    [(pair? expr)
     (apply (compute (car expr))
            (map (lambda (x) (compute x))
                 (cdr expr)))]
    [else (error "invalid expression: " expr)]))

;; === examples ===

(define expression-1
  '(+ 1
      (* 2
         3)))

(define expression-2
  '(+ (+ 1
         2)
      4))

(define expression-3
  '(+ (√ 25)
      2))

(compute expression-1) ; -> 7
(compute expression-2) ; -> 7
(compute expression-3) ; -> 7
