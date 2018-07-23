--
-- Instances.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.PolyLine.Instances where

import Geometry.Types

import Geometry.Curve.Class
import Geometry.Region.Class

import Geometry.Affine
import Geometry.Affine.Instances

import Geometry.PolyLine

import Data.Array

import Control.Lens ((^.))


polygonInside :: Polygon -> Point -> Bool
polygonInside poly pt = case poly^.region of
  Just convex ->
    inside () convex pt
  Nothing ->
    (approxWindingNumber () (poly^.boundary) pt) /= 0

polygonWindingNumber :: Polygon -> Point -> Integer
polygonWindingNumber poly pt = case poly^.region of
  Just convex ->
    if inside () convex pt
    then
      1
    else
      0
  Nothing ->
    approxWindingNumber () (poly^.boundary) pt

instance Curve PolyLine where
  type CurveData PolyLine = ()
  param pl t = 
    let
      arr = pl^.plAsArray
      (l,r) = bounds arr
      ts = t*(fromIntegral (r-l)) -- rescaled to match 0-1 per segment of the polyline
      ix = floor $ ts -- index of beginning of segment
      t' = ts - fromIntegral ix -- index in segment
    in
      param (makeSegment (arr!ix) (arr!(ix+1))) t'
  polyLine () = id
  distance () = polyLineDistance
  winding () = polyLineWinding

instance Curve Polygon where
  type CurveData Polygon = ()
  param poly t = param (poly^.boundary) t
  polyLine () poly = poly^.boundary
  distance () poly pt = polyLineDistance (poly^.boundary) pt
  -- TODO: update this
  winding () poly pt = fromIntegral $ polygonWindingNumber poly pt --polyLineWinding polyBdry pt

instance ClosedCurve Polygon where
  insideCC () = polygonInside
  windingNumber () = polygonWindingNumber


instance Region Polygon where
  type RegionData Polygon = ()
  inside = insideCC
  distanceBdry = Just distance



