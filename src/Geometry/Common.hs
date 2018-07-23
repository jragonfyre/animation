--
-- Common.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Common where

import Data.Array
import Geometry.Types

{-
segmentFold :: (Segment -> a) -> (a -> b -> b) -> b -> PolyLine -> b
segmentFold f op init (PolyLine arr) = 
  let
    ixs = range $ bounds arr
    segixs = zip ixs $ tail ixs
    segvals = map (\(ix1,ix2) -> f (arr ! ix1, arr! ix2)) segixs
  in
    foldr op init segvals

segmentFold1 :: (Segment -> a) -> (a -> a -> a) -> PolyLine -> a
segmentFold1 f op (PolyLine arr) = 
  let
    ixs = range $ bounds arr
    segixs = zip ixs $ tail ixs
    segvals = map (\(ix1,ix2) -> f (arr ! ix1, arr! ix2)) segixs
  in
    foldr1 op segvals
-}

