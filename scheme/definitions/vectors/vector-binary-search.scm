;; (vector-binary-search xs target compare) -> index | #f
;;  where xs      = (vector any), sorted: ascending
;;        target  = value
;;        compare = predicate (x less than y?)
;;
;; Performs binary search for a value within a vector
;; using the provided predicate function. Returns
;; either the index of the found value or boolean #f.
(define vector-binary-search
  (lambda (xs target compare)
    (let loop ([low  0]
               [high (- (vector-length xs) 1)])
      (if (> low high)
          ;; Target not found.
          #f
          (let* ([middle (quotient (+ low high) 2)]
                 [x      (vector-ref xs middle)])
            (cond [(compare x target)
                   ;; Search upper half.
                   (loop (+ middle 1) high)]
                  [(compare target x)
                   ;; Search lower half.
                   (loop low (- middle 1))]
                  ;; Target found. Return index.
                  [else
                   x]))))))

;; (vector-find-number xs target) -> index | #f
;;  where xs     = (vector number), sorted: ascending
;;        target = number
(define vector-find-number
  (lambda (xs target)
    (vector-binary-search xs target <)))

;; (vector-find-fixnum xs target) -> index | #f
;;  where xs     = (vector fixnum), sorted: ascending
;;        target = fixnum
(define vector-find-fixnum
  (lambda (xs target)
    (vector-binary-search xs target fx<?)))

;; (vector-find-flonum xs target) -> index | #f
;;  where xs     = (vector flonum), sorted: ascending
;;        target = flonum
(define vector-find-flonum
  (lambda (xs target)
    (vector-binary-search xs target fl<?)))

;; (vector-find-char xs target) -> index | #f
;;  where xs     = (vector char), sorted: ascending
;;        target = char
(define vector-find-char
  (lambda (xs target)
    (vector-binary-search xs target char<?)))

;; (vector-find-string xs target) -> index | #f
;;  where xs     = (vector string), sorted: lexical, ascending
;;        target = string
(define vector-find-string
  (lambda (xs target)
    (vector-binary-search xs target string<?)))
