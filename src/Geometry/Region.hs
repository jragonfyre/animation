--
-- Region.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Region
  ( module Geometry.Region
  , module Geometry.Region.Class
  ) where

import Geometry.Curve
import Geometry.Region.Class

  

instance Region Polygon where
  type RegionData Polygon = ()
  inside = insideCC
  distanceBdry = Just distance

instance Region Circle where
  type RegionData Circle = ()
  inside () = insideCC 0.0
  distanceBdry = Just $ \() -> distance 0.0 

instance Region ConvexPolytope where
  type RegionData ConvexPolytope = ()
  inside _ (ConvexPolytope hps) pt = and $ map (flip (inside ()) pt) hps
  distanceBdry = Nothing


instance Region ImplicitRegion where
  type RegionData ImplicitRegion = ()
  inside _ (ImplicitRegion f) pt = f pt
  distanceBdry = Nothing


{-
instance (ClosedCurve c) => Region c where
instance Region ClosedCurve where
  inside = insideCC
  distanceBdry = Just distance
-}

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
instance Region HalfPlane where
  type RegionData HalfPlane = ()
  inside _ (HalfPlane n d) pt = n `dot` pt >= d
  distanceBdry = Just $ \_ (HalfPlane n d) pt -> abs ((n `dot` pt)-d)
