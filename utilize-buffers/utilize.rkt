#lang racket
(define pattern #rx"[uU]tili([zs]e|[zs]ed|[zs]ing)")

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
                   (lambda (match other)
                     (hash-ref dictionary match))))

(define IN "text.txt")
(define OUT (string-append "re-" IN))

; === write file ===
(call-with-input-file IN
  (lambda (input)
    (call-with-output-file OUT
      (lambda (output)
        (let loop ([line (read-line input 'any)])
          (unless (eof-object? line)
            (write (replace line) output))))
      #:exists 'truncate)))
