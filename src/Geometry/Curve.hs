--
-- Curve.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve where

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

instance Curve Segment where
  type CurveData Segment = ()
  param (p1,p2) = line p1 p2
  polyLine () (p1,p2) = makePolyLine [p1,p2]
  distance () = segmentDistance
  winding () = segmentWinding


instance Curve Circle where
  --type CurveData Circle = ()
  param circle t = 
    let
      theta = t*2*pi
    in
      (radius circle * sin theta, radius circle * cos theta)
  polyLine = approxPolyLine
  --TODO: fix the exact methods
  --exactDistance = Just $ \circ pt -> abs $ radius circ - distancePt pt (center circ)
  --exactWinding = Just $ \circ pt -> 
    --if distancePt pt (center circ) <= radius circ 
    --then
      --1
    --else 
      --0


polyLineDistance :: PolyLine -> Point -> Double
polyLineDistance pl pt = segmentFold1 (flip segmentDistance pt) min pl

polyLineWinding :: PolyLine -> Point -> Double
polyLineWinding pl pt = segmentFold (flip segmentWinding pt) (+) 0 pl

instance Curve PolyLine where
  type CurveData PolyLine = ()
  param (PolyLine arr) t = 
    let
      (l,r) = bounds arr
      ts = t*(fromIntegral (r-l)) -- rescaled to match 0-1 per segment of the polyline
      ix = floor $ ts -- index of beginning of segment
      t' = ts - fromIntegral ix -- index in segment
    in
      param (arr ! ix,arr!(ix+1)) t'
  polyLine () = id
  distance () = polyLineDistance
  winding () = polyLineWinding

instance Curve Polygon where
  type CurveData Polygon = ()
  param Polygon{..} t = param polyBdry t
  polyLine () Polygon{..} = polyBdry
  distance () Polygon{..} pt = polyLineDistance polyBdry pt
  -- TODO: update this
  winding () Polygon{..} pt = polyLineWinding polyBdry pt


polygonWindingNumber :: Polygon -> Point -> Integer
polygonWindingNumber Polygon{..} pt = case polyRegion of
  Just convex ->
    if inside () convex pt
    then
      1
    else
      0
  Nothing ->
    approxWindingNumber () polyBdry pt

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

instance ClosedCurve Circle where
  windingNumber _ = \circ pt ->
    if radius circ >= distancePt pt (center circ)
    then
      1
    else
      0

instance ClosedCurve Polygon where
  insideCC () = polygonInside
  windingNumber () = polygonWindingNumber

polygonInside :: Polygon -> Point -> Bool
polygonInside Polygon{..} pt = case polyRegion of
  Just convex ->
    inside () convex pt
  Nothing ->
    (approxWindingNumber () polyBdry pt) /= 0


