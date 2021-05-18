; Scheme: R5RS
(define map-list
  (lambda (action lst)
    (cond
      ((null? lst) '())
       (else (cons (action (car lst))
                   (map-list action (cdr lst)))))))

(define result
  (map-list (lambda (number) (* number number))
            (list 1 2 3)))
; returns (1 4 9)
