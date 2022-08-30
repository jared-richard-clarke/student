(library (angles)
         (export deg->rad
                 rad->deg
                 PI
                 π)
         (import (rnrs base))
         
         ;; PI and π: approximation of π.

         (define PI 3.141592653589793)
         (define π PI)

         ;; (deg->rad number) -> number
         ;; Converts degrees to radians.
         ;; (deg->rad 7) -> 0.12217304763960307

         (define (deg->rad degrees)
           (* degrees (/ PI 180)))

         ;; (rad->deg number) -> number
         ;; Converts radians to degrees.
         ;; (rad->deg 0.12217304763960307) -> 7.0

         (define (rad->deg radians)
           (* radians (/ 180 PI)))

         )
