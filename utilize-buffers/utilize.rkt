#lang racket
(define pattern #rx"[uU]tili(?:[zs]e|[zs]ed|[zs]ing)")

(define dictionary
  #hash(["utilize" . "use"]
        ["utilise" . "use"]
        ["Utilize" . "Use"]
        ["Utilise" . "Use"]
        ["utilizes" . "uses"]
        ["utilises" . "uses"]
        ["utilized" . "used"]
        ["utilised" . "used"]
        ["utilizing" . "using"]
        ["utilising" . "using"]
        ["Utilizing" . "Using"]
        ["Utilising" . "Using"]))

(define (replace text) 
  (regexp-replace* pattern
                   text
                   (lambda (match)
                     (hash-ref dictionary match))))

; === write file ===
(define IN (open-input-file "text.txt"))
(define OUT (open-output-file "re-text.txt" #:exists 'truncate))

(define (stream input output)
  (let ([line (read-line input 'any)])
    (unless (eof-object? line)
      (write (replace line) output)
      (stream input output))))

(stream IN OUT)

(close-input-port IN)
(close-output-port OUT)

(when (and (port-closed? IN) (port-closed? OUT))
  (print "Stream complete. Files closed."))
