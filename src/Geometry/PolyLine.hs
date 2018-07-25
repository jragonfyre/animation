--
-- PolyLine.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.PolyLine where

import Geometry.Types
--import Geometry.Common
import Geometry.Affine

import Control.Lens
import Control.Lens.Fold (minimumOf,sumOf)

import Data.Maybe (fromJust)

polyLineDistance :: PolyLine -> Point -> Double
polyLineDistance pl pt = fromJust $ minimumOf (segments.to (flip segmentDistance pt)) pl

polyLineWinding :: PolyLine -> Point -> Double
polyLineWinding pl pt = sumOf (segments.to (flip segmentWinding pt)) pl




