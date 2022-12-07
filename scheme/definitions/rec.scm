;; (rec var expr) -> value
;; Creates a recursive expression from "expr" by binding
;; "var" within "expr" to the value of "expr".

(define-syntax rec
  (syntax-rules ()
    [(_ x e)
     (letrec ([x e]) x)]))

;; === example ===
;;
;; (map (rec sum
;;        (lambda (x)
;;          (if (= x 0)
;;              0
;;              (+ x (sum (- x 1))))))
;;      '(0 1 2 3 4 5))
;;
;; returns '(0 1 3 6 10 15)
