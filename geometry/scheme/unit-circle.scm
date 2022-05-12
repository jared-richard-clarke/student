;; (unit-circle symbol) -> pair
;; For a series of angles, returns the corresponding coordinates of a unit circle.
;; (unit-circle '90deg)  -> #(0 1)
;; (unit-circle '180deg) -> #(-1 0)

(define unit-circle
  (let ([table '([0deg   #(1 0)]
                 [30deg  #(0.866 0.5)]
                 [45deg  #(0.707 0.707)]
                 [60deg  #(0.5 0.866)]
                 [90deg  #(0 1)]
                 [120deg #(-0.5 0.866)]
                 [135deg #(-0.707 0.707)]
                 [150deg #(-0.866 0.5)]
                 [180deg #(-1 0)]
                 [210deg #(-0.866 -0.5)]
                 [225deg #(-0.707 -0.707)]
                 [240deg #(-0.5 -0.866)]
                 [270deg #(0 -1)]
                 [300deg #(0.5 -0.866)]
                 [315deg #(0.707 -0.707)]
                 [330deg #(0.866 -0.5)])])
    (lambda (key)
      (let ([value (assq key table)])
        (if (eq? value #f)
            (error "invalid key: " key)
            (cadr value))))))

;; === testing ===

;; (assert-equal expression value) -> current-output-port
;; If expression does not evaluate to value, macro prints failed test to current-output-port.
;; (assert-equal (vec2 4 4) #(3 4)) ->
;; Test: (vec2 4 4)
;; Expect: #(3 4), Got: #(4 4)

(define-syntax assert-equal
  (syntax-rules ()
    [(_ expression value)
     (let ([computed-expr expression]) ;; <- prevents redundant computation
       (when (not (equal? computed-expr value))
         (printf "Test: ~a\nExpect: ~a, Got: ~a\n"
                 (quote expression) ;; <---- returns expression prior to evaluation
                 value
                 computed-expr)))]))

;; === unit tests ===
(assert-equal (unit-circle '90deg)
              #(0 1))
(assert-equal (unit-circle '225deg)
              #(-0.707 -0.707))
