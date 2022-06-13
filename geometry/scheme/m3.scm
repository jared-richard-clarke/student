(define (m3-multiply a b)
  (let ([a00 (vector-ref a (+ (* 0 3) 0))]
        [a01 (vector-ref a (+ (* 0 3) 1))]
        [a02 (vector-ref a (+ (* 0 3) 2))]
        [a10 (vector-ref a (+ (* 1 3) 0))]
        [a11 (vector-ref a (+ (* 1 3) 1))]
        [a12 (vector-ref a (+ (* 1 3) 2))]
        [a20 (vector-ref a (+ (* 2 3) 0))]
        [a21 (vector-ref a (+ (* 2 3) 1))]
        [a22 (vector-ref a (+ (* 2 3) 2))]
        [b00 (vector-ref b (+ (* 0 3) 0))]
        [b01 (vector-ref b (+ (* 0 3) 1))]
        [b02 (vector-ref b (+ (* 0 3) 2))]
        [b10 (vector-ref b (+ (* 1 3) 0))]
        [b11 (vector-ref b (+ (* 1 3) 1))]
        [b12 (vector-ref b (+ (* 1 3) 2))]
        [b20 (vector-ref b (+ (* 2 3) 0))]
        [b21 (vector-ref b (+ (* 2 3) 1))]
        [b22 (vector-ref b (+ (* 2 3) 2))])
    (vector (+ (* b00 a00) (* b01 a10) (* b02 a20))
            (+ (* b00 a01) (* b01 a11) (* b02 a21))
            (+ (* b00 a02) (* b01 a12) (* b02 a22))
            (+ (* b10 a00) (* b11 a10) (* b12 a20))
            (+ (* b10 a01) (* b11 a11) (* b12 a21))
            (+ (* b10 a02) (* b11 a12) (* b12 a22))
            (+ (* b20 a00) (* b21 a10) (* b22 a20))
            (+ (* b20 a01) (* b21 a11) (* b22 a21))
            (+ (* b20 a02) (* b21 a12) (* b22 a22)))))

(define (m3-identity)
  (vector 1 0 0
          0 1 0
          0 0 1))

(define (m3-translate x y)
  (vector 1 0 0
          0 1 0
          x y 1))

(define (m3-rotate angle)
  (let ([c (cos angle)]
        [s (sin angle)])
    (vector c (- s) 0
            s    c  0
            0    0  1)))

(define (m3-scale x y)
  (vector x 0 0
          0 y 0
          0 0 1))
