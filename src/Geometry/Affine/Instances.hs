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

import Control.Lens ((^.))

instance Curve Segment where
  type CurveData Segment = ()
  param seg = line (seg^.start) (seg^.end)
  polyLine () seg = makePolyLine [seg^.start,seg^.end]
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
      ptFromPair (circle^.radius * sin theta, circle^.radius * cos theta)
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
  implicit circ pt = (distancePt pt (circ^.center)) - circ^.radius

instance ClosedCurve Circle where
  windingNumber _ = \circ pt ->
    if circ^.radius >= distancePt pt (circ^.center)
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




