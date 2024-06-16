;; (compare-vectors function vectors ...) -> boolean
;; Compares zero or more vectors by an equality predicate.
;; If all vectors are of the same length, and all elements
;; satisfy the predicate, the function returns #t.
;; Between zero and one vector, the function returns #t
;; regardless of predicate. Returns #f otherwise.
;; (compare-vectors = '#(1 2 3) '#(1 2 3)) -> #t

(define compare-vectors
  (lambda (predicate . vs)
    (cond
      [(null? vs) #t]
      [(null? (cdr vs)) #t]
      [else (and (let* ([v1 (car vs)]
                        [v2 (cadr vs)]
                        [size (vector-length v1)])
                   (and (= size (vector-length v2))
                        (let loop ([index 0])
                          (if (>= index size)
                              #t
                              (and (predicate (vector-ref v1 index) (vector-ref v2 index))
                                   (loop (+ index 1)))))))
                 (apply compare-vectors predicate (cdr vs)))])))
