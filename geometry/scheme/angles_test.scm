(import (scheme) (angles) (utils))

;; === unit tests ===

(assert-equal (deg->rad 7) 0.12217304763960307)
(assert-equal (rad->deg 0.12217304763960307) 7.0)
