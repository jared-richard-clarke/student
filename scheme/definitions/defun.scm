;; Replicates the "defun" syntax from Common Lisp
;;
;; (defun add (x y) (+ x y)) ->
;; (define add (lambda (x y) (+ x y)))

(define-syntax defun
    (lambda (stx)
      (syntax-case stx ()
        [(_ x y z)
         (syntax (define x (lambda y z)))])))
