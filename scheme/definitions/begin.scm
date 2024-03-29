;; (begin x y ...) -> y
;; Evaluates each expression left to right, returning only the value of the
;; last expression evaluated.
;;
;; (begin (display 7)
;;        (display 11)
;;        (+ 7 11))
;;
;; === expands ===>
;;
;; ((lambda (ignore)
;;   ((lambda (ignore)
;;      (+ 7 11))
;;    (display 11)))
;;  (display 7))

(define-syntax begin
  (syntax-rules ()
    [(_ x) x]
    [(_ x y ...)
     ((lambda (ignore) (begin y ...)) x)]))
