;; The factorial of a positive integer is the product
;; of all positive integers less than or equal to itself.

;; Recursive definition: n! = n * (n - 1)!

;; Factorial function for positive integers.
(define (factorial number)
    (if (= number 1)
        1
        (* number (factorial (- number 1)))))

;; Equivalent expressions
(factorial 3)
(* 3 (factorial 2))
(* 3 (* 2 (factorial 1)))
(* 3 (* 2 1))
(* 3 2)
6