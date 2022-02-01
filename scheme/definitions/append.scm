; function: (append list ... obj)
; purpose: concatenates lists
; (append '(1 2 3) '()) -> '(1 2 3)
; (append '(1 2) '(3 4)) -> '(1 2 3 4)

(define append
  (lambda args
    (let f ([lst '()]
            [args args])
      (if (null? args)
          lst
          (let g ([lst lst])
            (if (null? lst)
                (f (car args)
                   (cdr args))
                (cons (car lst)
                      (g (cdr lst)))))))))
