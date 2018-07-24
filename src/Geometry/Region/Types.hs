--
-- Types.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Region.Types
  ( module Geometry.Region.Types
  ) where

import Geometry.Types

import Control.Lens

data Region = Region
  { _regionInside :: Point -> Bool
  , _regionDistanceToBdry :: Maybe (Point -> Double)
  }

makeRegion :: (Point -> Bool) -> Region
makeRegion f = Region f Nothing

makeFields ''Region



