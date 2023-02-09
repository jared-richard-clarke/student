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
hypot :: Floating t => t -> t -> t
hypot x y = sqrt $ x ** 2 + y ** 2

-- 2D vector
data Vec2 t = Vec2 t t
  deriving (Eq, Show, Read)

add :: Num t => Vec2 t -> Vec2 t -> Vec2 t
add (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)

sub :: Num t => Vec2 t -> Vec2 t -> Vec2 t
sub (Vec2 a b) (Vec2 c d) = Vec2 (a - c) (b - d)

negate :: Num t => Vec2 t -> Vec2 t
negate (Vec2 x y) = Vec2 (0 - x) (0 - y)

abs :: (Num t, Ord t) => Vec2 t -> Vec2 t
abs (Vec2 x y) = Vec2 (absolute x) (absolute y)
  where
    absolute n = if n < 0 then 0 - n else n

invert :: Fractional t => Vec2 t -> Vec2 t
invert (Vec2 x y) = Vec2 (1 / x) (1 / y)

sum :: (Foldable s, Num t) => s (Vec2 t) -> Vec2 t
sum = foldr1 add

magnitude :: Floating t => Vec2 t -> t
magnitude (Vec2 x y) = hypot x y

scale :: Num t => Vec2 t -> t -> Vec2 t
scale (Vec2 x y) n = Vec2 (n * x) (n * y)

dot :: Num t => Vec2 t -> Vec2 t -> t
dot (Vec2 a b) (Vec2 c d) = a * c + b * d

distance :: Floating t => Vec2 t -> Vec2 t -> t
distance (Vec2 a b) (Vec2 c d) =
  let x = c - a
      y = d - b
   in hypot x y

lerp :: Num t => Vec2 t -> Vec2 t -> t -> Vec2 t
lerp (Vec2 a b) (Vec2 c d) n = Vec2 (interp a c n) (interp b d n)
  where
    interp x y t = (y - x) * t + x

normalize :: Floating t => Vec2 t -> Vec2 t
normalize v =
  let Vec2 x y = v
      m = magnitude v
   in Vec2 (x / m) (y / m)

transform :: Num t => Vec2 t -> Matrix t -> Vec2 t
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
