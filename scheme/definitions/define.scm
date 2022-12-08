;; (def add (lambda (x y) (+ x y))) ->
;; (define add (lambda (x y) (+ x y)))

(define-syntax def
  (syntax-rules ()
    [(_ x y ...)
     (define x y ...)]))

;; (defun add (x y) (+ x y)) ->
;; (define add (lambda (x y) (+ x y)))

(define-syntax defun
  (syntax-rules ()
    [(_ x y z)
     (define x (lambda y z))]))
