module Utils (hypot) where

import Data.Foldable (foldl')

hypot :: (Floating a, Foldable t) => t a -> a
hypot xs = sqrt $ foldl' (\acc x -> acc + x ** 2) 0 xs
