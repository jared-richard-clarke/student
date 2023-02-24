#lang racket

;; Copied from the Racket Library's list.rkt module.
;; (define-lgetter second 2) -> (define (second l0) ...)
(define-syntax define-lgetter
  (syntax-rules ()
    [(_ name npos)
     (define (name l0)
       (if (list? l0)
           (let loop ([l l0]
                      [pos npos])
             (if (pair? l)
                 (if (eq? pos 1)
                     (car l)
                     (loop (cdr l)
                           (sub1 pos)))
                 (raise-arguments-error 'name
                                        "list contains too few elements"
                                        "list" l0)))
           (raise-argument-error 'name "list?" l0)))]))

(define-lgetter second  2)
(define-lgetter third   3)
(define-lgetter fourth  4)
(define-lgetter fifth   5)
(define-lgetter sixth   6)
(define-lgetter seventh 7)
(define-lgetter eighth  8)
(define-lgetter ninth   9)
(define-lgetter tenth   10)

;; list-getter definitions rewritten using partial application and functional composition.
(define (list-getter name npos)
  (lambda (lst)
    (if (list? lst)
        (let loop ([l lst]
                   [pos npos])
          (if (pair? l)
              (if (eq? pos 1)
                  (car l)
                  (loop (cdr l)
                        (sub1 pos)))
              (raise-arguments-error name
                                     "list contains too few elements"
                                     "list" lst)))
        (raise-argument-error name "list?" lst))))

(define 2nd  (list-getter '2nd 2))
(define 3rd  (list-getter '3rd 3))
(define 4th  (list-getter '4th 4))
(define 5th  (list-getter '5th 5))
(define 6th  (list-getter '6th 6))
(define 7th  (list-getter '7th 7))
(define 8th  (list-getter '8th 8))
(define 9th  (list-getter '9th 9))
(define 10th (list-getter '10th 10))
