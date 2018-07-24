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
  , module Geometry.Curve
  , module Geometry.Curve.Types
  , module Geometry.Region
  , module Geometry.Region.Types
  , module Geometry.Affine
--  , module Geometry.Affine.Instances
  , module Geometry.PolyLine
--  , module Geometry.PolyLine.Instances
  , module Geometry.Bezier
  ) where

import Data.Array 

import Geometry.Types
import Geometry.Constructors
import Geometry.Common

import Geometry.Curve
import Geometry.Curve.Types
import Geometry.Region
import Geometry.Region.Types

import Geometry.Affine
--import Geometry.Affine.Instances
import Geometry.PolyLine
--import Geometry.PolyLine.Instances

import Geometry.Bezier









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

