; Scheme: R5RS
; --- syntax definitions ---

(define-syntax function
  (syntax-rules ()
    ((_ x y ...)
     (lambda x y ...))))

(define-syntax procedure
  (syntax-rules ()
    ((_ x y ...)
     (lambda x y ...))))

; --- function definitions ---

(define square
  (lambda (x) (* x x)))

(define cube
  (function (x) (expt x 3)))

(define factorial
  (procedure (x) (if (= x 0)
                     1
                     (* x (factorial (- x 1))))))
