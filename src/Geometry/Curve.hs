--
-- Curve.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve
  ( module Geometry.Curve
  , module Geometry.Types
  ) where

import Geometry.Types


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


