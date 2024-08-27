;; Evaluates Scheme arithmetic expressions.
;; (compute '(+ 1 2 3 4 5 6 7 8 9 10)) -> 55

(define compute
  (let ([table (list [cons '+ +]
                     [cons '- -]
                     [cons '* *]
                     [cons '× *]
                     [cons '/ /]
                     [cons '÷ /]
                     [cons '^ expt]
                     [cons '√ sqrt]
                     [cons 'π 3.141592653589793]
                     [cons 'e 2.718281828459045]
                     [cons 'φ 1.618033988749894])]
        [lookup (lambda (var env)
                  (let ([result (assq var env)])
                    (if result
                        (cdr result)
                        (error "undefined operator or constant:" var))))])
    (lambda (expression)
      (cond
        [(symbol? expression) (lookup expression table)]
        [(number? expression) expression]
        [(pair? expression)
         (apply (compute (car expression))
                (map compute (cdr expression)))]
        [else (error "invalid expression:" expression)]))))
