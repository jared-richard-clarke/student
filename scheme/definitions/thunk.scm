;; (thunk expression) -> (lambda () expression)
;; Wraps an expression in a function via macro for delayed evaluation.
;; (thunk (+ 1 6)) -> (lambda () (+ 1 6))

(define-syntax thunk
  (syntax-rules ()
    [(_ e)
     (lambda () e)]))
