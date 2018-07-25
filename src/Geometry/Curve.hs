--
-- Curve.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve 
  ( module Geometry.Curve
  , module Geometry.Curve.Types
  ) where

import Data.Array

import Geometry.Types
--import Geometry.Constructors
--import Geometry.Common

import Geometry.Curve.Types
import Geometry.Region.Types

import Geometry.Affine
import Geometry.PolyLine

import Control.Lens

segmentCurve :: Segment -> Curve
segmentCurve seg = 
  let 
    st = seg^.start
    ed = seg^.end
    pl = (makePolyLine [st,ed])
    bb = polyLineBoundingBox pl
  in
    makeCurve
      (line st ed)
      pl
      bb
      (Just $ \pt ->
        let
          (n,d)=segmentToLine seg
        in
          (n `dot` pt) - d
      )
      (Just $ segmentDistance seg)
      (Just $ segmentWinding seg)

circleParametrization :: Circle -> Parametrization
circleParametrization circle t = 
  let
    theta = t*2*pi
  in
    ptFromPair (circle^.radius * sin theta, circle^.radius * cos theta)

circleImplicitization :: Circle -> Implicitization
circleImplicitization circ pt = 
  (distancePt pt (circ^.center)) - circ^.radius

circleCurve :: Double -> Circle -> Curve
circleCurve d circle = (circleCCurve d circle)^.re _Closed

circleCCurve :: Double -> Circle -> ClosedCurve
circleCCurve d circ = 
  let
    param = circleParametrization circ
    imp = circleImplicitization circ
    r = circ^.radius
    rrvec = (r,r)^.from vecAsPair
    bbllc = circ^.center -. rrvec
  in
    makeClosedCurve
      param
      (approximateCC param evenApproximator d)
      (makeBox bbllc ((2::Double)*.rrvec))
      (Just imp)
      (Just $ abs . imp)
      (Just $ \pt -> if imp pt <= 0 then 1 else 0)
      (Just $ \pt -> if imp pt <= 0 then 1 else 0)
      (Just $ \pt -> if imp pt <= 0 then True else False)



