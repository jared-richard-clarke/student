;; (generate-numbers number number? number?) -> function -> number | 'end
;; Returns a function that yields a number incremented until it reaches
;; the end of its specified range. Every subsequent call returns the symbol 'end.
;;
;; (define yield (generate-numbers 2))     -> (list (yield) (yield) (yield)) -> '(1 2 end)
;; (define yield (generate-numbers 1 2))   -> (list (yield) (yield) (yield)) -> '(1 2 end)
;; (define yield (generate-numbers 1 6 5)) -> (list (yield) (yield) (yield)) -> '(1 6 end)

(define generate-numbers
  (let ([END 'end])
    (case-lambda
      [(stop)
       (generate-numbers 1 stop 1)]
      [(start stop)
       (generate-numbers start stop 1)]
      [(start stop step)
       (if (<= step 0)
           END
           (lambda ()
             (if (> start stop)
                 END
                 (let ([result start])
                   (set! start (+ start step))
                   result))))])))
