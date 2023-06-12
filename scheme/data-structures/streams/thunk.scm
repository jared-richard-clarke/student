;; Lazy evaluation implemented using thunks.

(define-syntax stream
  (syntax-rules ()
    [(_ x y)
     (cons x (lambda () y))]))

(define stream-ref
  (lambda (s n)
    (if (<= n 1)
        (car s)
        (stream-ref ((cdr s)) (- n 1)))))

(define alternate
  (lambda (n)
    (stream n (alternate (let ([x (+ (abs n) 1)])
                           (if (negative? n)
                               x
                               (- x)))))))

;; === expands ===>
;;
;; (define alternate
;;   (lambda (n)
;;     (cons n (lambda ()
;;               (alternate (let ([x (+ (abs n) 1)])
;;                            (if (negative? n) x (- x))))))))


(stream-ref (alternate 1) 10) ;; -> -10
(stream-ref (alternate 1) 9)  ;; ->   9
(stream-ref (alternate 1) 8)  ;; -> - 8
