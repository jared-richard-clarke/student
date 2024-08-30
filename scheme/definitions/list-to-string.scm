;; (list->string (list char)) -> string
;; Converts a list of characters into a string.
;; (list->string '(#\a #\b #\c)) -> "abc"
(define list->string
  (lambda (xs)
    (call-with-string-output-port
     (lambda (output)
       (for-each (lambda (x) (write-char x output)) xs)))))
