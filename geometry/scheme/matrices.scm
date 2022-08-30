;; Provides affine transformation matrices and functions.

(library (matrices)
         (export mat3
                 m3-multiply
                 m3-identity
                 m3-translate
                 m3-scale
                 m3-rotate
                 m3-shear
                 m3-transform)
         (import (rnrs base)
                 (rnrs lists))

         ;; (mat3 |number * 6|) -> (vector |number * 6|)
         ;; A 2d transformation implemented as a column-major, 3 × 3 matrix. The third row is implicit.

         (define (mat3 xx yx xy yy x0 y0)
           (vector xx yx
                   xy yy
                   x0 y0))


         ;; (m3-multiply mat3 mat3) -> mat3
         ;; Combines matrix transformations through multiplication.

         (define (m3-multiply b a)
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

         ;; (m3-identity) -> mat3
         ;; Creates an identity matrix.

         (define (m3-identity)
           (mat3 1 0
                 0 1
                 0 0))

         ;; (m3-translate number number) -> mat3
         ;; Creates a translation matrix.

         (define (m3-translate x y)
           (mat3 1 0
                 0 1
                 x y))

         ;; (m3-scale number number) -> mat3
         ;; Creates a scaling matrix.

         (define (m3-scale x y)
           (mat3 x 0
                 0 y
                 0 0))

         ;; (m3-rotate number) -> mat3
         ;; Creates a rotation matrix. Argument, "angle", measured in radians.

         (define (m3-rotate angle)
           (let* ([c (cos angle)]
                  [s (sin angle)])
             (mat3    c  s
                   (- s) c
                      0  0)))

         ;; (m3-shear number number) -> mat3
         ;; Creates a shearing matrix.

         (define (m3-shear x y)
           (mat3 1 y
                 x 1
                 0 0))

         ;; (m3-transform mat3 ...) -> mat3
         ;; Multiplies a list of transformation matrices pairwise to create a combined transform.
         ;; If no matrices are provided, function returns a mat3-identity matrix.

         (define (m3-transform . matrices)
           (let ([len (length matrices)])
             (cond
               [(= len 0) (m3-identity)]
               [(= len 1) (car matrices)]
               [else (fold-left m3-multiply
                                (car matrices)
                                (cdr matrices))])))

         )
