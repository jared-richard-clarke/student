module Vectors
  ( Vector (..),
    add,
    sub,
    invert,
    magnitude,
    scale,
    dotProduct,
    distance,
    lerp,
    normalize,
    transform,
  )
where

import Matrices (Matrix (..))

data Vector = Vec2 Double Double | Vec3 Double Double Double
  deriving (Eq, Show)

add :: Vector -> Vector -> Vector
add v1 v2 =
  case (v1, v2) of
    (Vec2 x1 y1, Vec2 x2 y2) -> Vec2 (x1 + x2) (y1 + y2)
    (Vec3 x1 y1 z1, Vec3 x2 y2 z2) -> Vec3 (x1 + x2) (y1 + y2) (z1 + z2)

sub :: Vector -> Vector -> Vector
sub v1 v2 =
  case (v1, v2) of
    (Vec2 x1 y1, Vec2 x2 y2) -> Vec2 (x1 - x2) (y1 - y2)
    (Vec3 x1 y1 z1, Vec3 x2 y2 z2) -> Vec3 (x1 - x2) (y1 - y2) (z1 - z2)

invert :: Vector -> Vector
invert v =
  case v of
    Vec2 x y -> Vec2 (1 / x) (1 / y)
    Vec3 x y z -> Vec3 (1 / x) (1 / y) (1 / z)

magnitude :: Vector -> Double
magnitude v =
  case v of
    Vec2 x y -> sqrt $ (x * x) + (y * y)
    Vec3 x y z -> sqrt $ (x * x) + (y * y) + (z * z)

scale :: Vector -> Double -> Vector
scale v n =
  case v of
    Vec2 x y -> Vec2 (x * n) (y * n)
    Vec3 x y z -> Vec3 (x * n) (y * n) (z * n)

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 =
  case (v1, v2) of
    (Vec2 x1 y1, Vec2 x2 y2) -> (x1 * x2) + (y1 * y2)
    (Vec3 x1 y1 z1, Vec3 x2 y2 z2) -> (x1 * x2) + (y1 * y2) + (z1 * z2)

distance :: Vector -> Vector -> Double
distance v1 v2 =
  case (v1, v2) of
    (Vec2 x1 y1, Vec2 x2 y2) ->
      let x = x2 - x1
          y = y2 - y1
       in sqrt $ (x * x) + (y * y)
    (Vec3 x1 y1 z1, Vec3 x2 y2 z2) ->
      let x = x2 - x1
          y = y2 - y1
          z = z2 - z1
       in sqrt $ (x * x) + (y * y) + (z * z)

lerp :: Vector -> Vector -> Double -> Vector
lerp v1 v2 n =
  let interp x y t = (y - x) * t + x
   in case (v1, v2) of
        (Vec2 x1 y1, Vec2 x2 y2) ->
          Vec2 (interp x1 x2 n) (interp y1 y2 n)
        (Vec3 x1 y1 z1, Vec3 x2 y2 z2) ->
          Vec3 (interp x1 x2 n) (interp y1 y2 n) (interp z1 z2 n)

normalize :: Vector -> Vector
normalize v =
  let m = magnitude v
   in case v of
        Vec2 x y -> Vec2 (x / m) (y / m)
        Vec3 x y z -> Vec3 (x / m) (y / m) (z / m)

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
