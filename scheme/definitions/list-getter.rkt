#lang racket

; Copied from the Racket Library's list.rkt module.
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

; I rewrote these definitions using functional composition.
(define (create-getter name npos)
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

(define 2nd (create-getter '2nd 2))
(define 3rd (create-getter '3rd 3))
(define 4th (create-getter '4th 4))
(define 5th (create-getter '5th 5))
(define 6th (create-getter '6th 6))
(define 7th (create-getter '7th 7))
(define 8th (create-getter '8th 8))
(define 9th (create-getter '9th 9))
(define 10th (create-getter '10th 10))
