(import (rnrs) (rnrs mutable-pairs))

;; meta-circular Scheme interpreter as defined by R. Kent Dybvig
;; in the fourth edition of The Scheme Programming Language — chapter 12.7

;; primitive-environment contains a small number of primitive procedures.
;; It can be extended easily with additional primitives.

(define primitive-environment
  (list (cons 'apply apply)
        (cons 'assq assq)
        (cons 'call/cc call/cc)
        (cons 'car car)
        (cons 'cadr cadr)
        (cons 'caddr caddr)
        (cons 'cadddr cadddr)
        (cons 'cddr cddr)
        (cons 'cdr cdr)
        (cons 'cons cons)
        (cons 'eq? eq?)
        (cons 'list list)
        (cons 'map map)
        (cons 'memv memv)
        (cons 'null? null?)
        (cons 'pair? pair?)
        (cons 'read read)
        (cons 'set-car! set-car!)
        (cons 'set-cdr! set-cdr!)
        (cons 'symbol? symbol?)))

;; new-env returns a new environment from a formal parameter specification,
;; a list of actual parameters, and an outer environment. The symbol? test
;; identifies "improper" argument lists. Environments are association lists,
;; associating variables with values.

(define new-env
  (lambda (formals actuals env)
    (cond
      [(null? formals) env]
      [(symbol? formals) (cons (cons formals actuals) env)]
      [else
       (cons
        (cons (car formals) (car actuals))
        (new-env (cdr formals) (cdr actuals) env))])))

;; lookup finds the value of the variable var in the environment
;; env, using assq. Assumes var is bound in env.

(define lookup
  (lambda (var env)
    (cdr (assq var env))))

;; assign is similar to lookup but alters the binding of the
;; variable var by changing the cdr of the association pair.

(define assign
  (lambda (var val env)
    (set-cdr! (assq var env) val)))

;; exec evaluates the expression, recognizing a small set of forms.

(define exec
  (lambda (expr env)
    (cond
      [(symbol? expr) (lookup expr env)]
      [(pair? expr)
       (case (car expr)
         [(quote) (cadr expr)]
         [(lambda)
          (lambda vals
            (let ([env (new-env (cadr expr) vals env)])
              (let loop ([exprs (cddr expr)])
                (if (null? (cdr exprs))
                    (exec (car exprs) env)
                    (begin
                      (exec (car exprs) env)
                      (loop (cdr exprs)))))))]
         [(if)
          (if (exec (cadr expr) env)
              (exec (caddr expr) env)
              (exec (cadddr expr) env))]
         [(set!) (assign (cadr expr) (exec (caddr expr) env) env)]
         [else
          (apply
           (exec (car expr) env)
           (map (lambda (x) (exec x env)) (cdr expr)))])]
      [else expr])))

;; interpret starts execution with the primitive environment.

(define interpret
  (lambda (expr)
    (exec expr primitive-environment)))
