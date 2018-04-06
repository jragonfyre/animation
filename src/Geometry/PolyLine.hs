--
-- PolyLine.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.PolyLine where

import Geometry.Types
import Geometry.Common
import Geometry.Affine


polyLineDistance :: PolyLine -> Point -> Double
polyLineDistance pl pt = segmentFold1 (flip segmentDistance pt) min pl

polyLineWinding :: PolyLine -> Point -> Double
polyLineWinding pl pt = segmentFold (flip segmentWinding pt) (+) 0 pl




