{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use negate" #-}
module Vectors
  ( Vec2 (..),
    add,
    sub,
    negate,
    abs,
    invert,
    sum,
    magnitude,
    scale,
    dot,
    distance,
    lerp,
    normalize,
    transform,
  )
where

import Matrices (Matrix (..))
import Prelude hiding (abs, negate, sum)

-- There's probably a more elegant solution to creating my own
-- definitions for "abs", "negate", and "sum"

-- utils
hypot :: Floating a => a -> a -> a
hypot x y = sqrt $ x ** 2 + y ** 2

-- 2D vector
data Vec2 a = Vec2 a a
  deriving (Eq, Show, Read)

add :: Num a => Vec2 a -> Vec2 a -> Vec2 a
add (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)

sub :: Num a => Vec2 a -> Vec2 a -> Vec2 a
sub (Vec2 a b) (Vec2 c d) = Vec2 (a - c) (b - d)

negate :: Num a => Vec2 a -> Vec2 a
negate (Vec2 x y) = Vec2 (0 - x) (0 - y)

abs :: (Num a, Ord a) => Vec2 a -> Vec2 a
abs (Vec2 x y) = Vec2 (absolute x) (absolute y)
  where
    absolute n = if n < 0 then 0 - n else n

invert :: Fractional a => Vec2 a -> Vec2 a
invert (Vec2 x y) = Vec2 (1 / x) (1 / y)

sum :: (Foldable t, Num a) => p -> t (Vec2 a) -> Vec2 a
sum xs = foldr1 add

magnitude :: Floating a => Vec2 a -> a
magnitude (Vec2 x y) = hypot x y

scale :: Num a => Vec2 a -> a -> Vec2 a
scale (Vec2 x y) n = Vec2 (n * x) (n * y)

dot :: Num a => Vec2 a -> Vec2 a -> a
dot (Vec2 a b) (Vec2 c d) = a * c + b * d

distance :: Floating a => Vec2 a -> Vec2 a -> a
distance (Vec2 a b) (Vec2 c d) =
  let x = c - a
      y = d - b
   in hypot x y

lerp :: Num a => Vec2 a -> Vec2 a -> a -> Vec2 a
lerp (Vec2 a b) (Vec2 c d) n = Vec2 (interp a c n) (interp b d n)
  where
    interp x y t = (y - x) * t + x

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v =
  let Vec2 x y = v
      m = magnitude v
   in Vec2 (x / m) (y / m)

transform :: Num a => Vec2 a -> Matrix a -> Vec2 a
transform v m =
  let Vec2 x y = v
      Mat3
        { a = a,
          b = b,
          c = c,
          d = d,
          e = _,
          f = _
        } = m
   in Vec2 (a * x + c * y) (b * x + d * y)
