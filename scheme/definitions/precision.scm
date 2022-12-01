;; "In [Scheme] implementations that use binary floating point, 
;;  the default precision can be calculated by calling [this procedure]"
;;
;; — R6RS Specification

(define (precision)
  (do ((n 0 (+ n 1))
       (x 1.0 (/ x 2.0)))
    ((= 1.0 (+ 1.0 x)) n)))
