module Shapes
  ( Quadrilateral (..),
    Circle,
    area,
    perimeter,
    length,
    width,
    diagonal,
    radius,
    diameter,
    circumference,
  )
where

import Prelude hiding (length)

hypot :: Floating a => a -> a -> a
hypot x y = sqrt $ x ^ 2 + y ^ 2

class Shape b where
  area :: (Num a, Floating a) => b a -> a

data Quadrilateral a = Rectangle a a | Square a

instance Shape Quadrilateral where
  area (Rectangle l w) = l * w
  area (Square s) = s ** 2

perimeter (Rectangle l w) = (l + w) * 2
perimeter (Square s) = s * 4

length :: Num a => Quadrilateral a -> a
length (Rectangle l w) = l
length (Square s) = s

width :: Num a => Quadrilateral a -> a
width (Rectangle l w) = w
width (Square s) = s

diagonal :: Floating a => Quadrilateral a -> a
diagonal (Rectangle l w) = hypot l w
diagonal (Square s) = hypot s s

newtype Circle a = Circle a

instance Shape Circle where
  area (Circle r) = 2 * pi * r

radius :: Num a => Circle a -> a
radius (Circle r) = r

diameter :: Num a => Circle a -> a
diameter (Circle r) = r * 2

circumference :: Floating a => Circle a -> a
circumference (Circle r) = pi * (r ** 2)
