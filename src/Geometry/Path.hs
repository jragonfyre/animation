--
-- Path.hs
-- Copyright (C) 2018 jragonfyre <jragonfyre@jragonfyre>
--
-- Distributed under terms of the MIT license.
--

module Geometry.Path
  ( module Geometry.Path
  ) where

import Geometry.Types
import Geometry.Bezier

import Data.Vector as V
import Data.List.Nonempty


data PathSegment
  = PathSeg !Point
  | PathBez2 !Point !Point
  | PathBez3 !Point !Point !Point

data WholePathSegment 
  = WPathSeg !Segment
  | WPathBez2 !Bezier2
  | WPathBez3 !Bezier3

newtype SimpleClosedPath
  = SCPath (Array Int PathSegment)

newtype OpenPath = OPath (Array Int PathSegment, Point)

-- isomorphic to nonempty really, maybe should use instead
-- I'm being lazy here
type ClosedPath = [SimpleClosedPath]

toWholeSegsSCP :: SimpleClosedPath -> Array Int WholePathSegment

toWholeSegsCP :: ClosedPath -> Array Int WholePathSegment
toWholeSegsCP = 

