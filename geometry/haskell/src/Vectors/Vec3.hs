{-# LANGUAGE InstanceSigs #-}

module Vectors.Vec3
  ( Vec3 (..),
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
  )
where

import Control.Applicative (liftA2)
import Prelude hiding (abs, negate, sum)

-- utils
hypot :: Floating t => t -> t -> t -> t
hypot x y z = sqrt $ x ** 2 + y ** 2 + z ** 2

-- 3D Vector
data Vec3 a = Vec3 !a !a !a
  deriving (Eq, Show, Read)

instance Functor Vec3 where
  fmap :: (a -> b) -> Vec3 a -> Vec3 b
  fmap fn (Vec3 x y z) = Vec3 (fn x) (fn y) (fn z)

instance Applicative Vec3 where
  pure :: a -> Vec3 a
  pure a = Vec3 a a a
  liftA2 :: (a -> b -> c) -> Vec3 a -> Vec3 b -> Vec3 c
  liftA2 fn v1 v2 = fn <$> v1 <*> v2

add :: Num a => Vec3 a -> Vec3 a -> Vec3 a
add = liftA2 (+)

sub :: Num a => Vec3 a -> Vec3 a -> Vec3 a
sub = liftA2 (-)

negate :: Num a => Vec3 a -> Vec3 a
negate = fmap (0 -)

abs :: (Num a, Ord a) => Vec3 a -> Vec3 a
abs = fmap absolute
  where
    absolute x = if x < 0 then (-x) else x

invert :: Fractional a => Vec3 a -> Vec3 a
invert = fmap (1 /)

sum :: (Foldable b, Num a) => b (Vec3 a) -> Vec3 a
sum = foldr add (Vec3 0 0 0)

magnitude :: Floating a => Vec3 a -> a
magnitude (Vec3 x y z) = hypot x y z

scale :: Num a => a -> Vec3 a -> Vec3 a
scale n = fmap (n *)

dot :: Num a => Vec3 a -> Vec3 a -> a
dot (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

distance :: Floating a => Vec3 a -> Vec3 a -> a
distance (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) =
  let x = x2 - x1
      y = y2 - y1
      z = z2 - z1
   in hypot x y z

lerp :: Num a => a -> Vec3 a -> Vec3 a -> Vec3 a
lerp t = liftA2 interp
  where
    interp x y = (y - x) * t + x

normalize :: Floating a => Vec3 a -> Vec3 a
normalize v = fmap (/ m) v where m = magnitude v
