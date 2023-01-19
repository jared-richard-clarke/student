;; (append-map function (list (list any) ...)) -> (list any)
;; Maps a function over a list of lists then applies append to the result.
;; (append-map reverse '((1 2) (3 4 5) (6 7))) -> '(2 1 5 4 3 7 6)

(define append-map
  (lambda (f xs)
    (apply append (map f xs))))
