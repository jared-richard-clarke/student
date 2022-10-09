(import (rnrs base)
        (matrices)
        (utils))

;; === unit tests ===

(assert-equal m3-ID
             (mat3 1 0 0 1 0 0))

(assert-equal (m3-compose m3-ID (m3-translate 3 4))
              (mat3 1 0 0 1 3 4))

(assert-equal (m3-compose m3-ID (m3-scale 2 2))
              (mat3 2 0 0 2 0 0))

(define m3-ID-float (mat3 1.0 0.0 0.0 1.0 0.0 0.0))

(assert-equal (m3-compose m3-ID-float
                         (m3-rotate (deg->rad 90))
                         (m3-rotate (deg->rad -90)))
               m3-ID-float)

(assert-equal (m3-compose m3-ID (m3-shear 3 4))
              (mat3 1 4 3 1 0 0))

(assert-equal (m3-compose (m3-translate 3 4) (m3-scale 2 2) (m3-shear 1 2))
              (mat3 2 4 2 2 3 4))
