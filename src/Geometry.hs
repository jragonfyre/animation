--
-- Geometry.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

-- 2D geometry module for use with animation
module Geometry 
  ( module Geometry
  , module Geometry.Types
  , module Geometry.Constructors
  , module Geometry.Common
  , module Geometry.Affine
  , module Geometry.Curve
  , module Geometry.Region
  ) where

import Data.Array 

import Geometry.Types
import Geometry.Constructors
import Geometry.Common

import Geometry.Affine
import Geometry.Curve
import Geometry.Region











{-
  approxPolygon :: Double -> c -> Polygon
  approxPolygon interval curve =
    let
      n = ceiling (1/interval) :: Integer
      delta = 1/ (fromIntegral n) :: Double
      f = \x -> param curve $ (fromIntegral x) * delta
    in
      makePolygon $ map f [0..(n-1)]
  distance :: Double -> c -> Point -> Double
-}

