;; (assert-test predicate expression value) -> current-output-port | (void)
;; If expression does not evaluate to value, macro prints failed test to the current-output-port.
;; (assert-test equal? (vec2 4 4) #(3 4)) ->
;; "Test: (vec2 4 4)
;;  Expect: #(3 4), Got: #(4 4)"

(define-syntax assert-test
  (lambda (stx)
    (syntax-case stx ()
      [(_ compare expression value)
       (syntax (let ([computed-expr expression]) ;; <- prevents redundant computation
                 (unless (compare computed-expr value)
                   (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
                           (quote expression) ;; <---- returns expression prior to evaluation
                           value
                           computed-expr))))])))

;; (assert-left-right predicate expression expression) -> current-output-port | (void)
;; If the left-hand-expression does not equal the right-hand-expression,
;; macro prints failed test to the current-output port.
;; (assert-left-right equal? (+ 1 6) (- 1 6)) ->
;; "Test failed:
;;  lhs: (+ 1 6) -> 7, rhs: (- 1 6) -> -5"

(define-syntax assert-left-right
  (lambda (stx)
    (syntax-case stx ()
      [(_ compare x y)
       (syntax (let ([computed-x x]
                     [computed-y y])
                 (unless (compare computed-x computed-y)
                   (printf "Test failed:\nlhs: ~a -> ~a, rhs: ~a -> ~a\n"
                           (quote x)
                           x
                           (quote y)
                           y))))])))
