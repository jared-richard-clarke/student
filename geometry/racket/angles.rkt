#lang racket

(provide deg->rad
         rad->deg)

(require "tests.rkt")

;; PI: approximation of π.

(define PI 3.141592653589793)

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

;; === unit tests ===

(assert-equal (deg->rad 7) 0.12217304763960307)
(assert-equal (rad->deg 0.12217304763960307) 7.0)
