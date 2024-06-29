;; Abstract object with message passing as defined in 
;; The Scheme Programming Language, by R. Kent Dybvig.

(define-syntax define-object
  (syntax-rules ()
    [(_ (name . init-states)
        ((property value) ...)
        ((method function) ...))
     (define name
       (lambda init-states
         (let* ([property value] ...)
           (letrec ([method function] ...)
             (lambda (message . arguments)
               (case message
                 [(method) (apply method arguments)] ...
                 [else (error 'name "invalid input: "
                              (cons message arguments))]))))))]
    [(_ (name . init-states)
        ((method function) ...))
     (define-object (name . init-states)
       ()
       ((method function) ...))]))

(define-object (stack)
  [(name 'stack)
   (state '())]
  [(type?  (lambda () name))
   (empty? (lambda () (null? state)))
   (push!  (lambda xs (set! state (append xs state))))
   (peek   (lambda () (car state)))
   (pop!   (lambda ()
             (let ([item (car state)])
               (set! state (cdr state))
               item)))
   (clear! (lambda () (set! state '())))])
