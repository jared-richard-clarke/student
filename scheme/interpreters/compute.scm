;; (compute expr) -> number
;; Evaluates arithmetic expressions in Scheme.
;; (compute '(+ 1 2 3 4 5 6 7 8 9 10)) -> 55

(define env
  (list (cons '+ +)
        (cons '- -)
        (cons '* *)
        (cons '× *)
        (cons '/ /)
        (cons '÷ /)))

(define (lookup var env)
  (cdr (assq var env)))

(define (compute expr)
  (cond
    [(symbol? expr) (lookup expr env)]
    [(number? expr) expr]
    [(pair? expr)
     (apply (compute (car expr))
            (map (lambda (x) (compute x))
                 (cdr expr)))]
    [else (error "invalid expression: " expr)]))
