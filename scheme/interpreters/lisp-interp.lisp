;; The first example of a LISP interpreter written in LISP,
;; according to Michael Fogus

(label eval (lambda (expr binds)
              (cond
                ((atom expr) (assoc expr binds))
                ((atom (car expr))
                 (cond
                   ((eq (car expr) (quote quote)) (cadr expr))
                   ((eq (car expr) (quote atom))  (atom  (eval (cadr expr) binds)))
                   ((eq (car expr) (quote eq))    (eq    (eval (cadr expr) binds)
                                                         (eval (caddr expr) binds)))
                   ((eq (car expr) (quote car))   (car   (eval (cadr expr) binds)))
                   ((eq (car expr) (quote cdr))   (cdr   (eval (cadr expr) binds)))
                   ((eq (car expr) (quote cons))  (cons  (eval (cadr expr) binds)
                                                         (eval (caddr expr) binds)))
                   ((eq (car expr) (quote cond))  (eval-cond (cdr expr) binds))
                   (t (eval (cons (assoc (car expr) binds)
                                  (cdr expr))
                            binds))))
                ((eq (caar expr) (quote label))
                 (eval (cons (caddar expr) (cdr expr))
                       (cons (list (cadar expr) (car expr)) binds)))
                ((eq (caar expr) (quote lambda))
                 (eval (caddar expr)
                       (append (pair (cadar expr) (eval-args (cdr expr) binds))
                               binds)))
                (t (assoc expr binds)))))
