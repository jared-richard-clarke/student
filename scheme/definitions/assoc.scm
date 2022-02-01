; function: (assoc obj list)
; purpose: Either finds first element of an associative list whose car equals obj or returns false.
; assv and assq are defined similarly, with eqv? and eq? in place of equal?
; (assoc 'b '((a . 1) (b . 2))) -> (b . 2)

(define assoc
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(equal? (caar ls) x) (car ls)]
      [else (assq x (cdr ls))])))

(define assq
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eq? (caar ls) x) (car ls)]
      [else (assq x (cdr ls))])))

(define assv
  (lambda (x ls)
    (cond
      [(null? ls) #f]
      [(eqv? (caar ls) x) (car ls)]
      [else (assq x (cdr ls))])))
