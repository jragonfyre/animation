--
-- Structure.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve.Structure
  ( module Geometry.Curve.Structure
  ) where
{-


data Curve = Curve
  { param :: Parametrization -- should start at 0 end at 1
  , implicit :: Maybe ImplicitCurve -- implicit is used to detect what side of the curve a pixel falls on.
  -- can be used instead of distance to gauge distance from the curve
  -- also useful to bound a region
  , polyLine :: PolyLine -- a polyline approximation to the curve
  , distance :: Point -> Double
  , winding :: Point -> Double
  , closed :: Maybe ClosedCurve
  }


data ClosedCurve = CCurve
  { polygon :: Polygon
  , windingNumber :: Point -> Integer
  , curve :: Curve
  }


evenApproximator :: ApproximationStrategy
evenApproximator interval 

-- expects 0, 1 to be in the list of points
approximateCurve :: Parametrization -> [Double] -> Curve
approximateCurve param points d = 
  let
    pl = makePolyLine $ map param points
  in
    Curve param Nothing pl (polyLineDistance pl) (polyLineWinding pl) Nothing

-- expects param 0 == param 1
approximateClosedCurve :: Parametrization -> [Double] -> Curve
approximateClosedCurve param points d = 
  let
    pl = makePolygon $ map param points
  in
    Curve param Nothing pl (polyLineDistance pl) (polyLineWinding pl) Nothing



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

class Curve c => ImplicitCurve c where
  implicit :: c -> Point -> Double

approxWindingNumber :: Curve c => CurveData c -> c -> Point -> Integer
approxWindingNumber val curve pt = round $ winding val curve pt

approxInside :: (ClosedCurve c) => CurveData c -> c -> Point -> Bool
approxInside val curve pt = (windingNumber val curve pt) /= 0

-}

