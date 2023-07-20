;; End flag for communication between producer and consumer.
(define END 'end)

;; producer
(define range
  (case-lambda
    [(stop)
     (range 1 stop 1)]
    [(start stop)
     (range start stop 1)]
    [(start stop step)
     (if (<= step 0)
         END
         (lambda ()
           (if (> start stop)
               END
               (let ([result start])
                 (set! start (+ start step))
                 result))))]))

;; consumer factory
(define consumer
  (lambda (op id)
    (lambda (yield)
      (let loop ([accum id]
                 [value (yield)])
        (if (eq? value END)
            accum
            (loop (op accum value)
                  (yield)))))))

;; consumers of range
(define product (consumer * 1))
(define sum     (consumer + 0))
