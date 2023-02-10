{-# HLINT ignore "Use negate" #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

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
hypot x y = sqrt $ x * x + y * y

-- 2D vector
data Vec2 a = Vec2 a a
  deriving (Eq, Show, Read)

instance Functor Vec2 where
  fmap :: (a -> b) -> Vec2 a -> Vec2 b
  fmap fn (Vec2 x y) = Vec2 (fn x) (fn y)

add :: Num a => Vec2 a -> Vec2 a -> Vec2 a
add (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 + x2) (y1 + y2)

sub :: Num a => Vec2 a -> Vec2 a -> Vec2 a
sub (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (x1 - x2) (y1 - y2)

negate :: Num a => Vec2 a -> Vec2 a
negate = fmap (0 -)

abs :: (Num a, Ord a) => Vec2 a -> Vec2 a
abs = fmap absolute
  where
    absolute n = if n < 0 then (-n) else n

invert :: Fractional a => Vec2 a -> Vec2 a
invert = fmap (1 /)

sum :: (Foldable b, Num a) => b (Vec2 a) -> Vec2 a
sum = foldr add $ Vec2 0 0

magnitude :: Floating a => Vec2 a -> a
magnitude (Vec2 x y) = hypot x y

scale :: Num a => a -> Vec2 a -> Vec2 a
scale n = fmap (n *)

dot :: Num a => Vec2 a -> Vec2 a -> a
dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

distance :: Floating a => Vec2 a -> Vec2 a -> a
distance (Vec2 x1 y1) (Vec2 x2 y2) =
  let x = x2 - x1
      y = y2 - y1
   in hypot x y

lerp :: Num a => a -> Vec2 a -> Vec2 a -> Vec2 a
lerp t (Vec2 x1 y1) (Vec2 x2 y2) = Vec2 (interp x1 x2) (interp y1 y2)
  where
    interp x y = (y - x) * t + x

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v = fmap (/ m) v where m = magnitude v

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
