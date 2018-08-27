--
-- Polynomial.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Polynomial
  ( module Polynomial
  ) where

import Geometry.Affine

type CPoly = Double
type LinPoly = (Double,Double)
type QuadPoly = (Double,Double,Double)
type CubPoly = (Double,Double,Double,Double)

instance Summable LinPoly LinPoly LinPoly where
  (+.) (a1,a0) (b1,b0) = (a1+b1,a0+b0)
instance Subtractable LinPoly LinPoly LinPoly where
  (-.) (a1,a0) (b1,b0) = (a1-b1,a0-b0)
instance Multiplicable Double LinPoly LinPoly where
  (*.) x (a1,a0) = (x*a1,x*a0)
instance Negatable LinPoly where
  negify (a1,a0) = (-a1,-a0)
instance Zeroable LinPoly where
  zero = (0,0)
instance Unitable LinPoly where
  unit = (0,1)
instance Summable QuadPoly QuadPoly QuadPoly where
  (+.) (a2,a1,a0) (b2,b1,b0) = (a2+b2,a1+b1,a0+b0)
instance Subtractable QuadPoly QuadPoly QuadPoly where
  (-.) (a2,a1,a0) (b2,b1,b0) = (a2-b2,a1-b1,a0-b0)
instance Multiplicable Double QuadPoly QuadPoly where
  (*.) x (a2,a1,a0) = (x*a2,x*a1,x*a0)
instance Negatable QuadPoly where
  negify (a2,a1,a0) = (-a2,-a1,-a0)
instance Zeroable QuadPoly where
  zero = (0,0,0)
instance Unitable QuadPoly where
  unit = (0,0,1)
instance Summable CubPoly CubPoly CubPoly where
  (+.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3+b3,a2+b2,a1+b1,a0+b0)
instance Subtractable CubPoly CubPoly CubPoly where
  (-.) (a3,a2,a1,a0) (b3,b2,b1,b0) = (a3-b3,a2-b2,a1-b1,a0-b0)
instance Multiplicable Double CubPoly CubPoly where
  (*.) x (a3,a2,a1,a0) = (x*a3,x*a2,x*a1,x*a0)
instance Negatable CubPoly where
  negify (a3,a2,a1,a0) = (-a3,-a2,-a1,-a0)
instance Zeroable CubPoly where
  zero = (0,0,0,0)
instance Unitable CubPoly where
  unit = (0,0,0,1)

evalLinear :: LinPoly -> Double -> Double
evalLinear (a1,a0) t = t*a1+a0

evalQuadratic :: QuadPoly -> Double -> Double
evalQuadratic (a2,a1,a0) t = t*(t*a2+a1)+a0

evalCubic :: CubPoly -> Double -> Double
evalCubic (a3,a2,a1,a0) t = t*(t*(t*a3+a2)+a1)+a0

derivCoeffsCubic :: CubPoly -> QuadPoly
derivCoeffsCubic (a3,a2,a1,_) = (3*a3,2*a2,a1)

derivCoeffsQuadratic :: QuadPoly -> LinPoly
derivCoeffsQuadratic (a2,a1,_) = (2*a2,a1)


