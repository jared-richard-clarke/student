# Quicksort

Also called a **partition-exchange sort**, quicksort is a divide-and-conquer algorithm. 
It works by selecting a **pivot** element from the array and partitioning the other elements 
into two sub-arrays, according to whether they are less than or greater than the pivot.

## Haskell

```haskell
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let left  = quicksort (filter (<= x) xs)  
        right = quicksort (filter (>  x) xs)   
    in  left ++ [x] ++ right 
```

## OCaml

```ocaml
let rec quicksort list =
  match list with
  | [] -> []
  | x::xs -> 
      let left = 
        xs 
        |> List.filter ((>) x) 
        |> quicksort
      and right = 
        xs 
        |> List.filter ((<=) x) 
        |> quicksort
      in 
      left @ [x] @ right
```

## Scheme

```scheme
(define (quick-sort compare lst)
  (if (null? lst)
      '()
      (let ([x  (car lst)]
            [xs (cdr lst)])
        (let ([left  (quick-sort compare (filter (lambda (y) (compare y x)) xs))]
              [right (quick-sort compare (filter (lambda (y) (not (compare y x))) xs))])
          (append left (list x) right)))))
```

## Clojure

```clojure
(defn quick-sort [[x & xs]]
  (if (nil? x)
      []
      (let [left  (quick-sort (filter #(<  % x) xs))
            right (quick-sort (filter #(>= % x) xs))]
        (concat left [x] right))))
```
