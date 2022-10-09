(define generate-numbers
  (case-lambda
    [(stop)
     (generate-numbers 1 stop 1)]
    [(start stop)
     (generate-numbers start stop 1)]
    [(start stop step)
     (unless (<= step 0)
       (lambda (message)
         (if (eq? message 'next)
             (unless (> start stop)
               (let ([result start])
                 (set! start (+ start step))
                 result))
             (error "call 'next to generate subsequent number"))))]))
