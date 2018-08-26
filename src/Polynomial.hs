--
-- Polynomial.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Polynomial
  ( module Polynomial
  ) where

type CPoly = Double
type LinPoly = (Double,Double)
type QuadPoly = (Double,Double,Double)
type CubPoly = (Double,Double,Double,Double)

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
