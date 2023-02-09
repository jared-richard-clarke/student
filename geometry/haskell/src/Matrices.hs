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

data Matrix = Mat3
  { a :: Double,
    b :: Double,
    c :: Double,
    d :: Double,
    e :: Double,
    f :: Double
  }
  deriving (Eq, Show, Read)

multiply :: Matrix -> Matrix -> Matrix
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

identity :: Matrix
identity =
  Mat3
    { a = 1.0,
      b = 0.0,
      c = 0.0,
      d = 1.0,
      e = 0.0,
      f = 0.0
    }

translate :: Double -> Double -> Matrix
translate x y =
  Mat3
    { a = 1.0,
      b = 0.0,
      c = 0.0,
      d = 1.0,
      e = x,
      f = y
    }

scale :: Double -> Double -> Matrix
scale x y =
  Mat3
    { a = x,
      b = 0.0,
      c = 0.0,
      d = y,
      e = 0.0,
      f = 0.0
    }

rotate :: Double -> Matrix
rotate angle =
  let x = cos angle
      y = sin angle
   in Mat3
        { a = x,
          b = y,
          c = negate x,
          d = y,
          e = 0.0,
          f = 0.0
        }

shear :: Double -> Double -> Matrix
shear x y =
  Mat3
    { a = 1.0,
      b = y,
      c = x,
      d = 1.0,
      e = 0.0,
      f = 0.0
    }

compose :: [Matrix] -> Matrix
compose = foldr multiply identity
