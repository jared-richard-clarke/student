;; (flat-map function (list any)) -> (list any)
;; Maps a function over a list of lists, then flattens that list by one level.
;; (flat-map (lambda (x) (list (+ (car x) 1))) '((1) (2) (3))) -> '(2 3 4)

;; Side Note: The monadic definition of "flat-map" is "bind".
;; flat-map: (a -> [b]) -> [a] -> [b]
;; bind:     [a] -> (a -> [b]) -> [b]

(define flat-map
  (lambda (fn xs)
    (fold-right append '() (map fn xs))))

;; -- equivalent ->

(define flat-map
  (lambda (fn xs)
    (if (null? xs)
        '()
        (append (fn (car xs)) (flat-map fn (cdr xs))))))
