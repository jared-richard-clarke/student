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

;; "prod" doesn't need a nonlocal exit to return early.
;; "prod" is tail-recursive. It need not return out of a nested stack.

(define (prod . lst)
  (let loop ([lst lst]
             [id 1])
    (cond
      [(null? lst) id]
      [(= (car lst) 0) 0] 
      [else (loop (cdr lst)
                  (* id (car lst)))])))
