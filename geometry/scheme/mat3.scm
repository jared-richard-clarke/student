(define (mat3 xx yx
              xy yy
              x0 y0)
  (vector xx yx
          xy yy
          x0 y0))

(define (multiply a b)
  (let ([a-xx (vector-ref a 0)]
        [a-yx (vector-ref a 1)]
        [a-xy (vector-ref a 2)]
        [a-yy (vector-ref a 3)]
        [a-x0 (vector-ref a 4)]
        [a-y0 (vector-ref a 5)]
        ; -------------------
        [b-xx (vector-ref a 0)]
        [b-yx (vector-ref a 1)]
        [b-xy (vector-ref a 2)]
        [b-yy (vector-ref a 3)]
        [b-x0 (vector-ref a 4)]
        [b-y0 (vector-ref a 5)])
    ; -------------------
    (mat3 [+ (* a-xx b-xx) (* a-yx b-xy)]
          [+ (* a-xx b-yx) (* a-yx b-yy)]
          [+ (* a-xy b-xx) (* a-yy b-xy)]
          [+ (* a-xy b-yx) (* a-yy b-yy)]
          [+ (* a-x0 b-xx) (* a-y0 b-xy) b-x0]
          [+ (* a-x0 b-yx) (* a-y0 b-yy) b-y0])))

(define (mat3-identity)
  (mat3 1 0
        0 1
        0 0))

(define (translate x y)
  (mat3 1 0
        0 1
        x y))

(define (scale x y)
  (mat3 x 0
        0 y
        0 0))

(define (rotate angle)
  (let* ([c (cos angle)]
         [s (sin angle)]
         [-s (- s)])
    (mat3 c  s
          -s c
          0  0)))

(define (shear x y)
  (mat3 1 y
        x 1
        0 0))

(define (mat3-translate mat x y)
  (multiply (translate x y) mat))

(define (mat3-scale mat x y)
  (multiply (scale x y) mat))

(define (mat3-rotate mat angle)
  (multiply (rotate angle) mat))

(define (mat3-shear mat x y)
  (multiply (shear x y) mat))
