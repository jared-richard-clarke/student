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

;; Yield numbers from 1 through 5000 piecemeal.
;; (define yield (generate-numbers 1 5000))

;; (list (yield 'next) (yield 'next) (yield 'next)) -> '(1 2 3)

;; (list (yield 'next) (yield 'next)) -> '(4 5)

;; ...5000 then nothing.
