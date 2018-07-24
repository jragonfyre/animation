--
-- Structure.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Curve.Types
  ( module Geometry.Curve.Types
  ) where

import Geometry.Types
import Geometry.Affine
import Geometry.PolyLine

import Data.Array
import Data.Maybe (maybe,fromMaybe)

import Control.Lens

import Utils

type Parametrization = Double -> Point  -- parametrization domain is [0,1]
type Implicitization = Point -> Double  
-- should be 0 at curve, and nonzero not at curve, sign should change across curve
type ApproximationStrategy = Double -> [Double]

data Curve = Curve
  { _curveParam :: Parametrization -- should start at 0 end at 1
  , curveImplicit :: Maybe Implicitization
  -- implicit is used to detect what side of the curve a pixel falls on
  -- can be used instead of distance to gauge distance from the curve
  -- also useful to bound the region which contains the curve to make coloring the curve more efficient
  -- determining the side the point falls on is also important for more efficiently determining whether a point
  -- is inside. (More efficient than the winding number I think, tho it should be about the same?)
  , _curvePolyLine :: PolyLine -- a polyline approximation to the curve
  , curveDistance :: Maybe (Point -> Double)
  , curveWinding :: Maybe (Point -> Double)
  , curveClosed :: Maybe ClosedCurve
  }

data ClosedCurve = CCurve
  { _closedCurvePolygon :: Polygon
  , closedCurveWindingNumber :: Maybe (Point -> Integer)
  , closedCurveInside :: Maybe (Point -> Bool)
  , closedCurveCurve :: Curve
  }

_Closed :: Prism' Curve ClosedCurve
_Closed = prism' closedCurveCurve curveClosed 

lensWithDefault :: (t -> a) -> (t -> Maybe a) -> (t -> a -> t) -> Lens' t a
lensWithDefault dGet mGet aSet = lens (\val -> fromMaybe (dGet val) (mGet val)) aSet

distance :: Lens' Curve (Point -> Double)
distance =
  lensWithDefault
    (polyLineDistance . _curvePolyLine) 
    curveDistance 
    (\curve dist -> fixClosedPtrs curve{curveDistance = Just dist})

winding :: Lens' Curve (Point -> Double)
winding =
  lensWithDefault
    (polyLineWinding . _curvePolyLine) 
    curveWinding 
    (\curve wind -> fixClosedPtrs curve{curveWinding = Just wind})

implicit :: Lens' Curve (Maybe Implicitization)
implicit = lens curveImplicit (\curve val -> fixClosedPtrs curve{curveImplicit = val})

polygonInside :: Polygon -> Point -> Bool
polygonInside poly pt = polygonWindingNumber poly pt /= 0

polygonWindingNumber :: Polygon -> Point -> Integer
polygonWindingNumber poly pt = case poly^.region of
  Just convex ->
    if insideConvtope convex pt
    then
      1
    else
      0
  Nothing ->
    approxWindingNumber (polyLineWinding (poly^.boundary)) pt

-- TODO: finish this
inside :: Lens' ClosedCurve (Point -> Bool)
inside =
  lensWithDefault
    (polygonInside . _closedCurvePolygon) 
    closedCurveInside 
    (\ccurve insd -> fixClosedPtrsCC ccurve{closedCurveInside = Just insd})

-- TODO: finish this too
windingNumber :: Lens' ClosedCurve (Point -> Integer)
windingNumber =
  lensWithDefault
    (polygonWindingNumber . _closedCurvePolygon) 
    closedCurveWindingNumber
    (\ccurve wnnm -> fixClosedPtrsCC ccurve{closedCurveWindingNumber = Just wnnm})

polyLineParametrization :: PolyLine -> Parametrization
polyLineParametrization pl t = 
  let
    arr = pl^.plAsArray
    (l,r) = bounds arr
    ts = t*(fromIntegral (r-l)) -- rescaled to match 0-1 per segment of the polyline
    ix = floor $ ts -- index of beginning of segment
    t' = ts - fromIntegral ix -- index in segment
  in
    line (arr!ix) (arr!(ix+1)) t'

polyLineCurve :: PolyLine -> Curve
polyLineCurve pl = buildCurveWithApproximation (polyLineParametrization pl) pl

polygonCCurve :: Polygon -> ClosedCurve
polygonCCurve poly = buildClosedCurveWithApproximation (poly^.boundary.to polyLineParametrization) poly

buildCurveWithApproximation :: Parametrization -> PolyLine -> Curve
buildCurveWithApproximation param pl =
    Curve
      { _curveParam = param
      , curveImplicit = Nothing
      , _curvePolyLine = pl
      , curveDistance = Nothing
      , curveWinding = Nothing
      , curveClosed = Nothing
      }

{-
buildCurveAndApproximate :: Parametrization -> [Double] -> Curve
buildCurveAndApproximate param ixPts =
  let
    pl = makePolyLine $ map param ixPts
  in
    buildCurveWithApproximation param pl
-}

fixClosedPtrs :: Curve -> Curve
fixClosedPtrs curve = case curveClosed curve of
  Nothing ->
    curve
  Just cc ->
    (pointToEachOther curve cc) ^.re _Closed

fixClosedPtrsCC :: ClosedCurve -> ClosedCurve
fixClosedPtrsCC ccurve = pointToEachOther (ccurve^.re _Closed) ccurve

pointToEachOther :: Curve -> ClosedCurve -> ClosedCurve
pointToEachOther oc occ = 
  let
    c = oc{curveClosed = Just cc}
    cc = occ{closedCurveCurve = c}
  in
    cc

approxWindingNumber :: (Point -> Double) -> (Point -> Integer)
approxWindingNumber = (round .)

buildClosedCurveWithApproximation :: Parametrization -> Polygon -> ClosedCurve
buildClosedCurveWithApproximation param poly = 
  let
    ccWinding = approxWindingNumber $ polyLineWinding (poly^.boundary)
    c = Curve
      { _curveParam = param
      , curveImplicit = Nothing
      , _curvePolyLine = poly^.boundary
      , curveDistance = Nothing
      , curveWinding = Nothing
      , curveClosed = Just cc
      }
    cc = CCurve
      { _closedCurvePolygon = poly
      , closedCurveWindingNumber = Nothing
      , closedCurveInside = Nothing
      , closedCurveCurve = c
      }
  in
    cc

makeLensesWith onlyGetters ''Curve
makeLensesWith onlyGetters ''ClosedCurve

evenApproximator :: ApproximationStrategy
evenApproximator interval = 
  let
    n = ceiling (1/interval)
  in
    map ((/fromIntegral n) . fromIntegral) [0..n]

approximate :: Parametrization -> ApproximationStrategy -> Double -> PolyLine
approximate param appStrat d = makePolyLine . map param $ appStrat d

approximateCC :: Parametrization -> ApproximationStrategy -> Double -> Polygon
approximateCC param appStrat d = flip makePolygon Nothing . makePolyLine . map param $ appStrat d

-- expects 0, 1 to be in the list of points
--approximateCurve :: Parametrization -> [Double] -> Curve
--approximateCurve param points d = 
  --let
    --pl = makePolyLine $ map param points
  --in
    --Curve param Nothing pl (polyLineDistance pl) (polyLineWinding pl) Nothing
--
---- expects param 0 == param 1
--approximateClosedCurve :: Parametrization -> [Double] -> Curve
--approximateClosedCurve param points d = 
  --let
    --pl = makePolygon $ map param points
  ----in
    --Curve param Nothing pl (polyLineDistance pl) (polyLineWinding pl) Nothing
------
--
