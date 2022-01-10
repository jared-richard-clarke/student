;; A minimal interpretor, written in Scheme
;; Presented by William Byrd
(load "pmatch.scm")
(define eval-expr
    (lambda (expr env)
        (pmatch expr
            [,x (guard (symbol? x))
             (env x)]
            [(lambda (,x) ,body)
             (lambda (arg)
                 (eval-expr body (lambda (y)
                                     (if (eq? x y)
                                         arg
                                         (env y)))))]
            [(,operator ,operand)
             ((eval-expr operator env)
              (eval-expr operand env))])))
