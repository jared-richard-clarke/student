;; (values any ...) -> any ...
;;
;; Returns a continuation of zero or more values.
;;
;; (define-values (x y) (values 3 4))
;; x -> 3
;; y -> 4
(define values
  (lambda xs
    (call/cc (lambda (k) (k xs)))))
