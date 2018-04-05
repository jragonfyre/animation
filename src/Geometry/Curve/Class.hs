--
-- Class.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve.Class
  ( module Geometry.Curve.Class
  ) where

import Geometry.Types
import Geometry.Constructors
import Geometry.PolyLine

-- a parametrizable curve
-- min implementation is param
class Curve c where
  type CurveData c :: *
  type CurveData c = Double
  param :: c -> Double -> Point 
  -- more general curves can find better ways to sample [0,1] to provide a better approximation of the curve
  -- e.g. more samples when there is greater curvature, or one sample per fixed length of curve, arc length
  -- parametrization would be ideal.
  -- also
  polyLine :: CurveData c -> c -> PolyLine
  --polyLine = approxPolyLine
  distance :: CurveData c -> c -> Point -> Double
  distance = approxDistance
  winding :: CurveData c -> c -> Point -> Double
  winding = approxWinding

approxPolyLine :: (Curve c) => Double -> c -> PolyLine
approxPolyLine interval curve =
  let
    n = ceiling (1/interval) :: Integer
    delta = 1/ (fromIntegral n) :: Double
    f = \x -> param curve $ (fromIntegral x) * delta
  in
    makePolyLine $ map f [0..n]

approxDistance :: (Curve c) => CurveData c -> c -> Point -> Double
approxDistance int curve = polyLineDistance (polyLine int curve)

approxWinding :: (Curve c) => CurveData c -> c -> Point -> Double
approxWinding int curve = polyLineWinding (polyLine int curve)

class Curve c => ClosedCurve c where
  insideCC :: CurveData c -> c -> Point -> Bool -- don't need to define if you define exactWindingNumber, unless
  -- you have a more efficient variant
  insideCC = approxInside
  windingNumber :: CurveData c -> c -> Point -> Integer
  windingNumber = approxWindingNumber

approxWindingNumber :: Curve c => CurveData c -> c -> Point -> Integer
approxWindingNumber val curve pt = round $ winding val curve pt

approxInside :: (ClosedCurve c) => CurveData c -> c -> Point -> Bool
approxInside val curve pt = (windingNumber val curve pt) /= 0

