;; (assert-test predicate expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-test equal? (vec2 4 4) #(3 4)) ->
;; Test: (vec2 4 4)
;; Expect: #(3 4), Got: #(4 4)

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
