(import (rnrs base)
        (matrices)
        (utils))

;; === unit tests ===

(assert-equal m3-ID
              (mat3 1 0 0 1 0 0))

(assert-equal (m3-multiply m3-ID (m3-translate 3 4))
              (mat3 1 0 0 1 3 4))

(assert-equal (m3-multiply m3-ID (m3-scale 2 2))
              (mat3 2 0 0 2 0 0))

(assert-equal (m3-multiply m3-ID (m3-rotate 5))
              (mat3 0.2836621854632263 -0.9589242746631385 0.9589242746631385 0.2836621854632263 0 0))

(assert-equal (m3-multiply m3-ID (m3-shear 3 4))
              (mat3 1 4 3 1 0 0))

(assert-equal (m3-transform (m3-translate 3 4) (m3-scale 2 2) (m3-shear 1 2))
              (mat3 2 4 2 2 3 4))
