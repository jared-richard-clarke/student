module Matrices
  ( Matrix (..),
    multiply,
    identity,
    translate,
    scale,
    rotate,
    shear,
    compose,
  )
where

import Data.Foldable (foldl')

data Matrix a = Mat3 {a, b, c, d, e, f :: !a}
  deriving (Eq, Show, Read)

multiply :: Num a => Matrix a -> Matrix a -> Matrix a
multiply m n =
  let Mat3
        { a = a1,
          b = b1,
          c = c1,
          d = d1,
          e = e1,
          f = f1
        } = m
      Mat3
        { a = a2,
          b = b2,
          c = c2,
          d = d2,
          e = e2,
          f = f2
        } = n
   in Mat3
        { a = a1 * a2 + b1 * c2,
          b = a1 * b2 + b1 * d2,
          c = c1 * a2 + d1 * c2,
          d = c1 * b2 + d1 * d2,
          e = e1 * a2 + f1 * c2 + e2,
          f = e1 * b2 + f1 * d2 + f2
        }

identity :: Num a => Matrix a
identity =
  Mat3
    { a = 1,
      b = 0,
      c = 0,
      d = 1,
      e = 0,
      f = 0
    }

translate :: Num a => a -> a -> Matrix a
translate x y =
  Mat3
    { a = 1,
      b = 0,
      c = 0,
      d = 1,
      e = x,
      f = y
    }

scale :: Num a => a -> a -> Matrix a
scale x y =
  Mat3
    { a = x,
      b = 0,
      c = 0,
      d = y,
      e = 0,
      f = 0
    }

rotate :: Floating a => a -> Matrix a
rotate angle =
  let x = cos angle
      y = sin angle
   in Mat3
        { a = x,
          b = y,
          c = negate x,
          d = y,
          e = 0,
          f = 0
        }

shear :: Num a => a -> a -> Matrix a
shear x y =
  Mat3
    { a = 1,
      b = y,
      c = x,
      d = 1,
      e = 0,
      f = 0
    }

compose :: (Foldable b, Num a) => b (Matrix a) -> Matrix a
compose = foldl' multiply identity
