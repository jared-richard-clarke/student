module Utils (hypot) where

import Data.Foldable (foldl')

hypot :: Floating t => [t] -> t
hypot = foldl' (\acc x -> acc + x ** 2) 0
