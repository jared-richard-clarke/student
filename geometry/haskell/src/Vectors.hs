module Vectors
  ( Vector (..),
    add,
    sub,
    neg,
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
import Prelude hiding (abs, sum)

-- There's probably a more elegant solution to creating my own
-- definitions for "abs" and "sum"

-- utils
hypot :: Double -> Double -> Double
hypot x y = sqrt $ x ** 2 + y ** 2

-- vectors
data Vector = Vec2 Double Double
  deriving (Eq, Show)

identity :: Vector
identity = Vec2 0.0 0.0

add :: Vector -> Vector -> Vector
add (Vec2 a b) (Vec2 c d) = Vec2 (a + c) (b + d)

sub :: Vector -> Vector -> Vector
sub (Vec2 a b) (Vec2 c d) = Vec2 (a - c) (b - d)

neg :: Vector -> Vector
neg (Vec2 x y) = Vec2 (negate x) (negate y)

abs :: Vector -> Vector
abs (Vec2 x y) = Vec2 (absolute x) (absolute y)
  where
    absolute n = if n < 0 then negate n else n

invert :: Vector -> Vector
invert (Vec2 x y) = Vec2 (1 / x) (1 / y)

sum :: Foldable t => p -> t Vector -> Vector
sum xs = foldr add identity

magnitude :: Vector -> Double
magnitude (Vec2 x y) = hypot x y

scale :: Vector -> Double -> Vector
scale (Vec2 x y) n = Vec2 (n * x) (n * y)

dot :: Vector -> Vector -> Double
dot (Vec2 a b) (Vec2 c d) = a * c + b * d

distance :: Vector -> Vector -> Double
distance (Vec2 a b) (Vec2 c d) =
  let x = c - a
      y = d - b
   in hypot x y

lerp :: Vector -> Vector -> Double -> Vector
lerp (Vec2 a b) (Vec2 c d) n = Vec2 (interp a c n) (interp b d n)
  where
    interp x y t = (y - x) * t + x

normalize :: Vector -> Vector
normalize v =
  let Vec2 x y = v
      m = magnitude v
   in Vec2 (x / m) (y / m)

transform :: Vector -> Matrix -> Vector
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

