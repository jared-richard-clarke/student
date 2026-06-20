;; (string-hashtable (key value) ...) -> string-hashtable
;;   where key   = string
;;         value = any
;; A convenience macro for building a string hashtable.
;;
;; (define age
;;   (string-hashtable ("Gladys" 100)
;;                     ("Susan"   65)
;;                     ("Britney" 35)
;;                     ("Brook"   18)))
(define-syntax string-hashtable
  (syntax-rules ()
    [(_ (key value) ...)
     (let* ([entries  (list (cons key value) ...)]
            [capacity (length entries)]
            [table    (make-hashtable string-hash string=? capacity)])
       (for-each (lambda (entry)
                   (let ([k (car entry)]
                         [v (cdr entry)])
                     (hashtable-set! table k v)))
                 entries)
       table)]))
