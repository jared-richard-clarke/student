;; (partial function values ...) -> (function values ...) -> value
;; Fixes a number of arguments to a function, producing another function with a smaller arity.
;; Applies function to combined arguments.
;; (define add1 (partial + 1)) -> (add1 10) -> 11

(define (partial fn . x)
  (lambda y
    (apply fn (append x y))))

;; Functions F2, F3, and F4 return partial functions implemented via currying.

;; (F2 function) ->  (function arg1) -> (function arg2) -> value
;; Two-argument function

(define (F2 fn)
  (lambda (a)
    (lambda (b)
      (fn a b))))

;; (F3 function) ->  (function arg1) -> (function arg2) -> (function arg3) -> value
;; Three-argument function

(define (F3 fn)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (fn a b c)))))

;; (F4 function) ->  (function arg1) -> (function arg2) -> (function arg3) -> (function arg4) -> value
;; Four-argument function

(define (F4 fn)
  (lambda (a)
    (lambda (b)
      (lambda (c)
        (lambda (d)
          (fn a b c d))))))

;; === examples ===

;; (define map-list (F2 map))

;; (define double (map-list (lambda (x) (* x 2))))
;; (define triple (map-list (lambda (x) (* x 3))))
;; (define shout  (map-list (lambda (x) (string-append x "!"))))

;; (double '(1 2 3)) -------> '(2 4 6)
;; (triple '(1 2 3)) -------> '(2 6 9)
;; (shout  '("cat" "dog")) -> '("cat!" "dog!")
