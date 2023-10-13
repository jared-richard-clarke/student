# List Monad

Old-school Haskell implementation of a list monad. Pattern matching drives list evaluation.

```haskell
instance Monad [] where
  -- result :: a -> [a]
  result x = [x]
  -- bind :: [a] -> (a -> [b]) -> [b]
  [] `bind` f     = []
  (x:xs) `bind` f = f x ++ (xs `bind` f)

instance Monad0Plus [] where
  -- zero :: [a]
  zero = []
  -- (++) :: [a] -> [a] -> [a]
  [] ++ ys     = ys
  (x:xs) ++ ys = x : (xs ++ ys)
```
