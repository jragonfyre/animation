--
-- Class.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Region.Class where

import Geometry.Types

class Region r where
  type RegionData r :: *
  type RegionData r = Double
  inside :: RegionData r -> r -> Point -> Bool
  distanceBdry :: Maybe (RegionData r -> r -> Point -> Double)


