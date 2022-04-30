;; PI: approximation of π.

(define PI 3.141592653589793)

;; (deg->rad number) -> number
;; Converts degrees to radians.
;; (deg->rad 7) -> 0.12217304763960307

(define (deg->rad degrees)
  (* degrees (/ PI 180)))

;; (rad->deg number) -> number
;; Converts radians to degrees.
;; (rad->deg 0.12217304763960307) -> 7.0

(define (rad->deg radians)
  (* radians (/ 180 PI)))

;; === testing ===

;; (assert-equal expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-equal (vec2 4 4) '(3 . 4)) ->
;; Test: (vec2 4 4)
;; Expect: (3 . 4), Got: (4 . 4)

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expression value)
     (when (not (equal? expression value))
       (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
               (quote expression)
               value
               expression))]))

;; === unit tests ===

(assert-equal (deg->rad 7) 0.12217304763960307)
(assert-equal (rad->deg 0.12217304763960307) 7.0)
