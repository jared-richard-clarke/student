;; Abstract object with message passing as defined in 
;; The Scheme Programming Language by R. Kent Dybvig.

;; The "define-object" macro defines procedures that create abstract objects.
;; These objects are opaque in that their state can only be accessed indirectly
;; through their methods via message passing.
(define-syntax define-object
  (syntax-rules ()
    [(_ (name . constructors)
        ((field data) ...)
        ((method function) ...))
     (define name
       (lambda constructors
         (let* ([field data] ...)
           (letrec ([method function] ...)
             (lambda (message . arguments)
               (case message
                 [(method) (apply method arguments)] ...
                 [else (assertion-violation 'name "invalid message" (cons message arguments))]))))))]
    [(_ (name . constructors)
        ((method function) ...))
     (define-object (name . constructors)
       ()
       ((method function) ...))]))

;; === stack object ===

(define-object (stack)
  [(id    'stack)
   (state '())]
  [(type   (lambda () id))
   (empty? (lambda () (null? state)))
   (push!  (lambda xs (set! state (append xs state))))
   (peek   (lambda () (car state)))
   (pop!   (lambda ()
             (let ([item (car state)])
               (set! state (cdr state))
               item)))
   (clear! (lambda () (set! state '())))])

;; - expands ->

(define stack
  (lambda ()
    (let* ([id 'stack]
           [state '()])
      (letrec ([type   (lambda () id)]
               [empty? (lambda () (null? state))]
               [push!  (lambda xs (set! state (append xs state)))]
               [peek   (lambda () (car state))]
               [pop!   (lambda () (let ([item (car state)])
                                    (set! state (cdr state))
                                    item))]
               [clear! (lambda () (set! state '()))])
        (lambda (message . arguments)
          (case message
            [(type)   (apply type arguments)]
            [(empty?) (apply empty? arguments)]
            [(push!)  (apply push! arguments)]
            [(peek)   (apply peek arguments)]
            [(pop!)   (apply pop! arguments)]
            [(clear!) (apply clear! arguments)]
            [else
             (assertion-violation 'stack "invalid input" (cons message arguments))]))))))
