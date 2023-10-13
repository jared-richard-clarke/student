# Haskell: Lazy Evaluation

## Strict Evaluation

```scheme
(define loop-forever (lambda () (loop-forever)))

;; eager evaluation -> infinity (stack overflow)
;; lazy evaluation  -> 7

((lambda (x) 7) (loop-forever))
```

## Pattern Matching

Pattern matching drives evaluation.

```haskell
repeat :: a -> [a]
repeat x = x : repeat x

take :: Int -> [a] -> [a]
take n _ | n <= 0 = []
take _ []         = []
take n (x:xs)     = x : take (n-1) xs

-- `repeat` will evaluate only as far as `take` pattern matches
-- on `(x:xs)`.

take 3 (repeat 7)

-- 7 : take (3-1) (repeat 7)
-- ...
-- 7 : 7 : 7 : []
```

## Space Usage

```haskell
foldl :: (b -> a -> b) -> [a] -> b
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

-- Since the value of the accumulator is not demanded until after
-- recursing through the entire list, the accumulator builds up
-- the unevaluated expression `(((0+1)+2)+3)`
  foldl (+) 0 [1,2,3]
= foldl (+) (0+1) [2,3]
= foldl (+) ((0+1)+2) [3]
= foldl (+) (((0+1)+2)+3) []
= (((0+1)+2)+3)
= ((1+2)+3)
= (3+3)
= 6

-- strict in its first argument
seq :: a -> b -> b

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x in seq z' foldl' f z' xs

  foldl' (+) 0 [1,2,3]
= foldl' (+) (0+1) [2,3]
= foldl' (+) 1 [2,3]
= foldl' (+) (1+2) [3]
= foldl' (+) 3 [3]
= foldl' (+) (3+3) []
= foldl' (+) 6 []
= 6
```

## Evaluation and Thunks

```
=== expression ===

(4, [1, 2])

=== evaluation / reduction ===

             >
unevaluated | thunk
             >
	     >
	    | (thunk, thunk)
WHNF        | (4, thunk)
	    | (4, 1:thunk)
	    | (4, 1:2:thunk)
	     >
	     >
Normal Form | (4, 1:2:[])
             >
```

## Build and Take

```haskell
take :: int -> [a] -> [a]
take _ []     = []
take n (x:xs) = if n <= 0 then [] else (x : take (n - 1) xs)

build :: Int -> Int -> Int
build x y = if x > y then [] else x:(build (x + 1) y)

take 3 (build­List 1 99)
take 3 (1 :(build­List 2 99))
1:(take 2 (build­List 2 99))
1:(take 2 (2 : (build­List 3 99)))
1:2:(take 1 (build­List 3 99))
1:2:(take 1 (3 : (build­List 4 99)))
1:2:3:(take 0 (build­List 4 99))
1:2:3:[]
```
