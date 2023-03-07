{-# LANGUAGE NamedFieldPuns #-}

module Vectors.Vec2
  ( Vec2 (..),
    add,
    (^+^),
    sub,
    (^-^),
    negate,
    abs,
    invert,
    magnitude,
    scale,
    (^*),
    (*^),
    dot,
    distance,
    lerp,
    normalize,
    transform,
  )
where

import Control.Applicative (liftA2)
import Matrices (Matrix (..))
import Prelude hiding (abs, negate)

-- utils
hypot :: Floating t => t -> t -> t
hypot x y = sqrt $ x ** 2 + y ** 2

-- 2D Vector
data Vec2 a = Vec2 !a !a
  deriving (Eq, Show, Read)

instance Functor Vec2 where
  fmap fn (Vec2 x y) = Vec2 (fn x) (fn y)

instance Applicative Vec2 where
  pure a = Vec2 a a
  Vec2 a b <*> Vec2 c d = Vec2 (a c) (b d)

instance Num a => Semigroup (Vec2 a) where
  (<>) = add

instance Num a => Monoid (Vec2 a) where
  mempty = Vec2 0 0

add :: Num a => Vec2 a -> Vec2 a -> Vec2 a
add = liftA2 (+)

(^+^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(^+^) = add

infixl 6 ^+^

sub :: Num a => Vec2 a -> Vec2 a -> Vec2 a
sub = liftA2 (-)

(^-^) :: Num a => Vec2 a -> Vec2 a -> Vec2 a
(^-^) = sub

infixl 6 ^-^

negate :: Num a => Vec2 a -> Vec2 a
negate = fmap (0 -)

abs :: (Num a, Ord a) => Vec2 a -> Vec2 a
abs = fmap absolute
  where
    absolute x = if x < 0 then (-x) else x

invert :: Fractional a => Vec2 a -> Vec2 a
invert = fmap (1 /)

magnitude :: Floating a => Vec2 a -> a
magnitude (Vec2 x y) = hypot x y

scale :: Num a => a -> Vec2 a -> Vec2 a
scale n = fmap (n *)

(*^) :: Num a => a -> Vec2 a -> Vec2 a
(*^) n = fmap (n *)

infixl 7 *^

(^*) :: Num a => Vec2 a -> a -> Vec2 a
(^*) v n = fmap (n *) v

infixl 7 ^*

dot :: Num a => Vec2 a -> Vec2 a -> a
dot (Vec2 x1 y1) (Vec2 x2 y2) = x1 * x2 + y1 * y2

distance :: Floating a => Vec2 a -> Vec2 a -> a
distance (Vec2 x1 y1) (Vec2 x2 y2) =
  let x = x2 - x1
      y = y2 - y1
   in hypot x y

lerp :: Num a => a -> Vec2 a -> Vec2 a -> Vec2 a
lerp t = liftA2 interp
  where
    interp x y = (y - x) * t + x

normalize :: Floating a => Vec2 a -> Vec2 a
normalize v = fmap (/ m) v where m = magnitude v

transform :: Num a => Vec2 a -> Matrix a -> Vec2 a
transform v m =
  let Vec2 x y = v
      Mat3 {a, b, c, d} = m
   in Vec2 (a * x + c * y) (b * x + d * y)
