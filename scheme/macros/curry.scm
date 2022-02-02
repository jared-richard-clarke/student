; (define add-1 (add 1))
; (define add-9 (add-1 8))
; (add-9 2) -> 11
; (((add 1) 4) 2) -> 7

; === base ===
(define (((add x) y) z)
  (+ x y z))

; === macro expansion ===
(define add-ex
  (lambda (x)
    (lambda (y)
      (lambda (z)
        (+ x y z)))))
