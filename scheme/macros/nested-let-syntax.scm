;; === nested let ===

(let* [(x 3)
       (y 4)
       (z (* x y))]
  z)

;; === let expansion ===

(let [(x 3)]
  (let [(y 4)]
    (let [(z (* x y))]
      (let []
        z))))

;; === lambda expansion ===

((lambda (x)
   ((lambda (y)
      ((lambda (z)
         ((lambda () z)))
       (* x y)))
    4))
 3)
