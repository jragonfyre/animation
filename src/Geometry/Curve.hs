--
-- Curve.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve 
  ( module Geometry.Curve
  , module Geometry.Curve.Class
  ) where

import Data.Array

import Geometry.Types
import Geometry.Constructors
import Geometry.Common

import Geometry.Curve.Class
import Geometry.Region.Class

import Geometry.Affine
import Geometry.PolyLine


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
  winding () poly pt = fromIntegral $ polygonWindingNumber poly pt --polyLineWinding polyBdry pt


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


