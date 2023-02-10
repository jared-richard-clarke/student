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

import Control.Applicative (liftA2)
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

instance Functor Vec2 where
  fmap :: (a -> b) -> Vec2 a -> Vec2 b
  fmap fn (Vec2 x y) = Vec2 (fn x) (fn y)

instance Applicative Vec2 where
  pure :: a -> Vec2 a
  pure x = Vec2 x x
  (<*>) :: Vec2 (a -> b) -> Vec2 a -> Vec2 b
  (<*>) = liftA2 id
  liftA2 :: (a -> b -> c) -> Vec2 a -> Vec2 b -> Vec2 c
  liftA2 fn v1 v2 = fmap fn v1 <*> v2

add :: Num t => Vec2 t -> Vec2 t -> Vec2 t
add = liftA2 (+)

sub :: Num t => Vec2 t -> Vec2 t -> Vec2 t
sub = liftA2 (-)

negate :: Num t => Vec2 t -> Vec2 t
negate = fmap (0 -)

abs :: (Num t, Ord t) => Vec2 t -> Vec2 t
abs = fmap absolute
  where
    absolute n = if n < 0 then (-n) else n

invert :: Fractional t => Vec2 t -> Vec2 t
invert = fmap (1 /)

sum :: (Foldable s, Num t) => s (Vec2 t) -> Vec2 t
sum = foldr add $ Vec2 0 0

magnitude :: Floating t => Vec2 t -> t
magnitude (Vec2 x y) = hypot x y

scale :: Num t => t -> Vec2 t -> Vec2 t
scale n = fmap (n *)

dot :: Num t => Vec2 t -> Vec2 t -> t
dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

distance :: Floating t => Vec2 t -> Vec2 t -> t
distance (Vec2 x1 y1) (Vec2 x2 y2) =
  let x = x2 - x1
      y = y2 - y1
   in hypot x y

lerp :: Num t => t -> Vec2 t -> Vec2 t -> Vec2 t
lerp n = liftA2 (interp n)
  where
    interp t x y = (y - x) * t + x

normalize :: Floating t => Vec2 t -> Vec2 t
normalize v = fmap (/ m) v where m = magnitude v

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
