;; Provides affine transformation matrices and functions.

(library (matrices)
         (export mat3
                 m3-ID
                 m3-translate
                 m3-scale
                 m3-rotate
                 m3-shear
                 m3-compose)
         (import (rnrs base)
                 (rnrs lists))

         ;; (mat3 |number * 6|) -> (vector |number * 6|)
         ;; A column-major, 3 × 3 affine transformation matrix implemented as a 6-part vector.
         ;; The third row is implicit.
         ;;
         ;; [ a b c d ] <- linear transformations
         ;; [ e f ] <----- translations
         ;;
         ;;        |-------|-------|---- implied
         ;; [ a b (0) c d (0) e f (1) ]
         ;;   0 1     2 3     4 5

         (define (mat3 a b c d e f)
           (vector a b c d e f))


         ;; (m3-multiply mat3 mat3) -> mat3
         ;; Combines matrix transformations through multiplication.

         (define (m3-multiply n m)
           (let ([v vector-ref])
             (mat3 [+ (* (v m 0) (v n 0)) (* (v m 1) (v n 2))]
                   [+ (* (v m 0) (v n 1)) (* (v m 1) (v n 3))]
                   [+ (* (v m 2) (v n 0)) (* (v m 3) (v n 2))]
                   [+ (* (v m 2) (v n 1)) (* (v m 3) (v n 3))]
                   [+ (* (v m 4) (v n 0)) (* (v m 5) (v n 2)) (v n 4)]
                   [+ (* (v m 4) (v n 1)) (* (v m 5) (v n 3)) (v n 5)])))

         ;; m3-ID
         ;; A 3 × 3 identity matrix constant.

         (define m3-ID 
           (mat3 1 0 0 1 0 0))

         ;; (m3-translate number number) -> mat3
         ;; Creates a translation matrix.

         (define (m3-translate x y)
           (mat3 1 0 0 1 x y))

         ;; (m3-scale number number) -> mat3
         ;; Creates a scaling matrix.

         (define (m3-scale x y)
           (mat3 x 0 0 y 0 0))

         ;; (m3-rotate number) -> mat3
         ;; Creates a rotation matrix. Argument, "angle", measured in radians.

         (define (m3-rotate angle)
           (let* ([c (cos angle)]
                  [s (sin angle)])
             (mat3 c s (- s) c 0  0)))

         ;; (m3-shear number number) -> mat3
         ;; Creates a shearing matrix.

         (define (m3-shear x y)
           (mat3 1 y x 1 0 0))

         ;; (m3-compose mat3 ...) -> mat3
         ;; Multiplies a list of transformation matrices pairwise to create a combined transform.
         ;; If no matrices are provided, function returns a mat3-identity matrix.

         (define (m3-compose . matrices)
           (let ([len (length matrices)])
             (cond
               [(= len 0) m3-ID]
               [(= len 1) (car matrices)]
               [else (fold-left m3-multiply
                                (car matrices)
                                (cdr matrices))])))

         )
