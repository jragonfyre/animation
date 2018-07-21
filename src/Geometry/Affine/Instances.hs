--
-- Instances.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Affine.Instances where

import Geometry.Types
import Geometry.Constructors

import Geometry.Curve.Class
import Geometry.Region.Class

import Geometry.Affine


instance Curve Segment where
  type CurveData Segment = ()
  param (p1,p2) = line p1 p2
  polyLine () (p1,p2) = makePolyLine [p1,p2]
  distance () = segmentDistance
  winding () = segmentWinding

instance ImplicitCurve Segment where
  implicit segt pt = 
    let
      (n,d)=segmentToLine segt
    in
      (n `dot` pt) - d

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

instance ImplicitCurve Circle where
  implicit circ (x,y) = x^2+y^2 - (radius circ)

instance ClosedCurve Circle where
  windingNumber _ = \circ pt ->
    if radius circ >= distancePt pt (center circ)
    then
      1
    else
      0

instance Region Circle where
  type RegionData Circle = ()
  inside () = insideCC 0.0
  distanceBdry = Just $ \() -> distance 0.0 

instance Region HalfPlane where
  type RegionData HalfPlane = ()
  inside _ (HalfPlane n d) pt = n `dot` pt >= d
  distanceBdry = Just $ \_ (HalfPlane n d) pt -> abs ((n `dot` pt)-d)

instance Region ConvexPolytope where
  type RegionData ConvexPolytope = ()
  inside _ (ConvexPolytope hps) pt = and $ map (flip (inside ()) pt) hps
  distanceBdry = Nothing




