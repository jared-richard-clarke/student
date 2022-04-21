#! /usr/bin/env racket
#lang racket

(require "lib.rkt")

;; grab first command-line argument
(define file
  (command-line #:program "utilize"
                #:args (filename.txt)
                filename.txt))

;; check if file exists
(when (not (file-exists? file))
  (raise-user-error "file does not exist in current directory"))

;; check file type
(when (not (path-has-extension? file ".txt"))
  (raise-user-error "argument must be plain text file: <example.txt>"))

;; open input and output files
(define IN (open-input-file file))
(define OUT (open-output-file (string-append "edit-" file)
                              #:exists 'truncate))

;; process text stream line by line
(stream IN OUT)

;; close files
(close-input-port IN)
(close-output-port OUT)

;; end program
(when (and (port-closed? IN) (port-closed? OUT))
  (println "Edit complete. Files closed."))
