# Haskell: Lazy Evaluation

## Applicative and Normal Order Evaluation

```
=== Lambda Calculus ===

-- applicative order -> infinity
-- normal order      -> 7

λx.7 infinity
```

## Eager and Lazy Evaluation

```scheme
;; === Scheme ===

(define loop-forever (lambda () (loop-forever)))

;; eager evaluation -> infinity (stack overflow)
;; lazy evaluation  -> 7

((lambda (x) 7) (loop-forever))
```

## Pattern Matching

Pattern matching drives evaluation.

### Example 1: Build and Take

```haskell
take :: int -> [a] -> [a]
take _ []     = []
take n (x:xs) = if n <= 0 then [] else (x : take (n - 1) xs)

build :: Int -> Int -> Int
build x y = if x > y then [] else (x : build (x + 1) y)

take 3 (build 1 99)
take 3 (1 : (build 2 99))
1:(take 2 (build­ 2 99))
1:(take 2 (2 : (build 3 99)))
1:2:(take 1 (build 3 99))
1:2:(take 1 (3 : (build 4 99)))
1:2:3:(take 0 (build 4 99))
1:2:3:[]
```

### Example 2: Take and Drop (Miranda)

```miranda
take 0 x = []
take (n+1) [] = []
take (n+1) (a:x) = a : take n x

drop 0 x = x
drop (n+1) [] = []
drop (n+1) (a:x) = drop n x

take n x ++ drop n x = x
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
