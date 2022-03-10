; function: (append list ... obj)
; purpose: concatenates lists. ("cons up" a list while "cdring" down another)
; (append '(1 2 3) '()) -> '(1 2 3)
; (append '(1 2) '(3 4)) -> '(1 2 3 4)

(define append
  (lambda args
    (let loop-1 ([lst '()]
                 [args args])
      (if (null? args)
          lst
          (let loop-2 ([lst lst])
            (if (null? lst)
                (loop-1 (car args)
                        (cdr args))
                (cons (car lst)
                      (loop-2 (cdr lst)))))))))
