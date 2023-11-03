# Parametric and Overloaded

## Parametric `quicksort`

`quicksort` is parameterized by the function `compare`.

```haskell
  quicksort :: (a -> a -> Bool) -> [a] -> [a]
  quicksort compare [] = []
  quicksort compare (x:xs) =
      let left  = quicksort compare [a | a <- xs, compare x]
          right = quicksort compare [a | a <- xs, not (compare x)]
      in left ++ [x] ++ right
```

## Overloaded `quicksort`

Functions `<=` and `>` are overloaded with the constraint that type variable
`a` falls within the type class `Ord`.

```haskell
  quicksort :: (Ord a) => [a] -> [a]    
  quicksort [] = []    
  quicksort (x:xs) =     
      let left  = quicksort [a | a <- xs, a <= x]  
          right = quicksort [a | a <- xs, a >  x]   
      in  left ++ [x] ++ right 
```
