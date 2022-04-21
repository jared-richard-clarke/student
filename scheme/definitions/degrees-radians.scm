(library (degrees-radians)
         (export deg->rad rad->deg)
         (import (rnrs base (6)))
         
         ;; PI: approximation of π.

         (define PI 3.141592653589793)

         ;; (deg->rad number) -> number
         ;; Converts degrees to radians.
         ;; (deg->rad 7) -> 0.12217304763960307

         (define (deg->rad degrees)
           (* degrees (/ PI 180)))

         ;; (rad->deg number) -> number
         ;; Converts radians to degrees.
         ;; (rad->deg 0.12217304763960307) -> 7

         (define (rad->deg radians)
           (* radians (/ 180 PI))))
