;; Nonlocal exit allows product to return immediately, without performing
;; the pending multiplications if a zero value is detected.

(define (product . lst)
  (call/cc
   (lambda (return)
     (let fn ([lst lst])
       (cond
         [(null? lst) 1]
         [(= (car lst) 0) (return 0)] ;; <------- nonlocal exit
         [else (* (car lst) (fn (cdr lst)))])))))
