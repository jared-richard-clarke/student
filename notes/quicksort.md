# Quicksort

Also called a **partition-exchange sort**, quicksort is a divide-and-conquer algorithm. 
It works by selecting a **pivot** element from the array and partitioning the other elements 
into two sub-arrays, according to whether they are less than or greater than the pivot.

## Haskell

```haskell
quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let left  = quicksort [a | a <- xs, a <= x]  
        right = quicksort [a | a <- xs, a >  x]   
    in  left ++ [x] ++ right 
    
-- or ...

import Data.List (partition)

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []
quicksort (x:xs) = let (left, right) = partition (<= x) xs
                   in quicksort left ++ [x] ++ right
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

## Ruby

```ruby
def quicksort(array)
    return array if array.size < 2
  
    pivot = array[0]
    less = array[1..].select { |element| element <= pivot }
    greater = array[1..].select { |element| element > pivot }
    return *quicksort(less), pivot, *quicksort(greater)
end
```

## Scheme

```scheme
(define (quicksort compare lst)
  (if (null? lst)
      '()
      (let ([x  (car lst)]
            [xs (cdr lst)])
        (let ([left  (quicksort compare (filter (lambda (y) (compare y x)) xs))]
              [right (quicksort compare (filter (lambda (y) (not (compare y x))) xs))])
          (append left (list x) right)))))
```

## Clojure

```clojure
(defn quicksort [[x & xs]]
  (if (nil? x)
      []
      (let [left  (quicksort (filter #(<  % x) xs))
            right (quicksort (filter #(>= % x) xs))]
        (concat left [x] right))))
```
