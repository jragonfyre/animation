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

import Geometry.Types

import Geometry.Curve
import Geometry.Region.Class

import Geometry.Affine

  


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

