#! /usr/bin/scheme --program

;; Script file treated as an RNRS top-level program.

(import (rnrs))
(let ([args (cdr (command-line))])
  (unless (null? args)
    (let-values ([(newline? args)
                  (if (equal? (car args) "-n")
                      (values #f (cdr args))
                      (values #t args))])
      (do ([args args (cdr args)] [sep "" " "])
          ((null? args))
        (display sep)
        (display (car args)))
      (when newline? (newline)))))
