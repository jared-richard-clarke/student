;; (assert-eq expression value) -> boolean
;; Asserts expression evaluates to value.
;; (assert-eq (list 1 2 3) '(1 2 3)) -> true

(define (assert-eq expression value)
  (equal? expression value))

;; (assert-ne expression value) -> boolean
;; Asserts expression does not evaluate to value.
;; (assert-ne '(1 (2 3)) '(1 2 3)) -> true

(define (assert-ne expression value)
  (not (equal? expression value)))

;; (assert-equal expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-equal (vec2 4 4) #(3 4)) ->
;; Test: (vec2 4 4)
;; Expect: #(3 4), Got: #(4 4)

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expression value)
     (let ([computed-expr expression]) ;; <- prevents redundant computation
       (unless (equal? computed-expr value)
         (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
                 (quote expression) ;; <---- returns expression prior to evaluation
                 value
                 computed-expr)))]))
