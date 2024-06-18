;; The "recur" syntactic form. Named "rec" in the Chez Scheme manual.
;; "recur" is a special case of "letrec" for self-recursive objects.

(define-syntax recur
  (syntax-rules ()
    [(_ x e) (letrec ((x e)) x)]))

;; Expanded definition of the "recur" syntactic form as defined within
;; the Nanopass framework.

(define-syntax recur
  (syntax-rules ()
    [(_ name fn) (letrec ([name fn]) name)]
    [(_ (name . xs) e1 e2 ...)
     (letrec ([name (lambda xs e1 e2 ...)]) name)]))

;; === example ===
;;
;; (map (recur sum
;;        (lambda (x)
;;          (if (= x 0)
;;              0
;;              (+ x (sum (- x 1))))))
;;      '(0 1 2 3 4 5))
;;
;; returns '(0 1 3 6 10 15)
