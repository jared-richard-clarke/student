(define (mat3 xx yx xy yy x0 y0)
  (vector xx yx
          xy yy
          x0 y0))

(define (m3-multiply a b)
  (let ([a-xx (vector-ref a 0)]
        [a-yx (vector-ref a 1)]
        [a-xy (vector-ref a 2)]
        [a-yy (vector-ref a 3)]
        [a-x0 (vector-ref a 4)]
        [a-y0 (vector-ref a 5)]
        ; -------------------
        [b-xx (vector-ref b 0)]
        [b-yx (vector-ref b 1)]
        [b-xy (vector-ref b 2)]
        [b-yy (vector-ref b 3)]
        [b-x0 (vector-ref b 4)]
        [b-y0 (vector-ref b 5)])
    ; ---------------------------------
    (mat3 [+ (* a-xx b-xx) (* a-yx b-xy)]
          [+ (* a-xx b-yx) (* a-yx b-yy)]
          [+ (* a-xy b-xx) (* a-yy b-xy)]
          [+ (* a-xy b-yx) (* a-yy b-yy)]
          [+ (* a-x0 b-xx) (* a-y0 b-xy) b-x0]
          [+ (* a-x0 b-yx) (* a-y0 b-yy) b-y0])))

(define (m3-identity)
  (mat3 1 0
        0 1
        0 0))

(define (m3-translate x y)
  (mat3 1 0
        0 1
        x y))

(define (m3-scale x y)
  (mat3 x 0
        0 y
        0 0))

(define (m3-rotate angle)
  (let* ([c (cos angle)]
         [s (sin angle)]
         [-s (- s)])
    (mat3 c  s
          -s c
          0  0)))

(define (m3-shear x y)
  (mat3 1 y
        x 1
        0 0))

(define (m3-transform . matrices)
  (let ([len (length matrices)])
    (cond
      [(= len 0) (m3-identity)]
      [(= len 1) (car matrices)]
      [else (foldl m3-multiply
                   (car matrices)
                   (cdr matrices))])))

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

(assert-equal (m3-identity)
              (mat3 1 0 0 1 0 0))

(assert-equal (m3-multiply (m3-identity) (m3-translate 3 4))
              (mat3 1 0 0 1 3 4))

(assert-equal (m3-multiply (m3-identity) (m3-scale 2 2))
              (mat3 2 0 0 2 0 0))

(assert-equal (m3-multiply (m3-identity) (m3-rotate 5))
              (mat3 0.2836621854632263 -0.9589242746631385 0.9589242746631385 0.2836621854632263 0 0))

(assert-equal (m3-multiply (m3-identity) (m3-shear 3 4))
              (mat3 1 4 3 1 0 0))

(assert-equal (m3-transform (m3-translate 3 4) (m3-scale 2 2) (m3-shear 1 2))
              (mat3 2 4 2 2 3 4))
