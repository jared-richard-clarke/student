;; (fibonacci number) -> number
;; A series of numbers in which each number is the sum of the two preceding numbers.
;; (fibonacci 4) -> 3 as in 1, 1, 2, 3

(define (fibonacci n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fibonacci (- n 1))
                 (fibonacci (- n 2))))))

;; === tree-recursion ===
;; Space consumption is exponential. Process expands like a tree.
;; In general, the number if steps required by a tree-recursive process
;; will be proportional to the number of nodes in the tree, while the space
;; will be proportional to the maximum depth of the tree.

;;   Depth: 3
;;   Steps: 5
;;                 fibonacci 3
;;                     /\
;;                    /  \
;;         fibonacci 2    fibonacci 1
;;                  / \             |
;;                 /   \            1
;;      fibonacci 1     fibonacci 0
;;                |               |
;;                1               0

;; fibonacci: iterative

(define (fibonacci-iter n)
  (let loop ([x 1]
             [y 0]
             [count n])
    (if (= count 0)
        y
        (loop (+ x y)
              x
              (- count 1)))))
