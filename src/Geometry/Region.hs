--
-- Region.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Region
  ( module Geometry.Region
  , module Geometry.Region.Types
  ) where

import Data.Maybe (fromMaybe)

import Geometry.Types

import Geometry.Curve
import Geometry.Region.Types

import Geometry.Affine

import Control.Lens ((^.), to, allOf, (.~), (&), re)


implicitRegion :: Implicitization -> (Double -> Bool) -> Region
implicitRegion f test = makeRegion (test . f)

isNegative :: Double -> Bool
isNegative = (<= 0)

isPositive :: Double -> Bool
isPositive = (>= 0)

withinTolerance :: Double -> Double -> Bool
withinTolerance tol = (< tol) . abs

closedCurveRegion :: ClosedCurve -> Region
closedCurveRegion cc = 
  (makeRegion (cc^.insideCurve))
  & distanceToBdry .~ (Just (cc^.re _Closed.distance))

circleRegion :: Circle -> Region
circleRegion circ = implicitRegion (circleImplicitization circ) isNegative

halfPlaneImplicitization :: HalfPlane -> Point -> Double
halfPlaneImplicitization hp pt = (hp^.normal) `dot` pt - hp^.radius

halfPlaneRegion :: HalfPlane -> Region
halfPlaneRegion hp = 
  let 
    imp = halfPlaneImplicitization hp
  in 
    (implicitRegion imp isPositive) 
    & distanceToBdry .~ (Just $ abs . imp)

convtopeRegion :: ConvexPolytope -> Region
convtopeRegion conv = makeRegion (\pt -> allOf hplanes (\hp -> (hp^.to halfPlaneRegion . inside) pt) conv)

polygonRegion :: Polygon -> Region
polygonRegion poly =
  let
    mcp = poly^.region
    cc = polygonCCurve poly
    mbreg = fmap convtopeRegion mcp
  in 
    (fromMaybe (makeRegion (cc^.insideCurve)) mbreg)
    & distanceToBdry .~ (Just (cc^.re _Closed.distance))

{-
-- distance from a point to a region
distanceTo :: (Region r) => RegionData r -> r -> Point -> Maybe Double
distanceTo val reg pt = 
  if not $ inside val reg pt
  then
    distanceBdry >>= (\f -> return $ f val reg pt)
  else
    Just 0

-- distance from a point in a region to the exterior of the region
distanceOut :: (Region r) => RegionData r -> r -> Point -> Maybe Double
distanceOut val reg pt = 
  if not $ inside val reg pt
  then
    distanceBdry >>= (\f -> return $ f val reg pt)
  else
    Just 0
-}

